{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

-- | The module provides 'asDag', 'asPaths' and 'asPath' wrapper functions
-- which use Morfeusz bindings to analyse input sentences.

module NLP.Morfeusz
(
-- * Types
  DAG
, Edge (..)
, Token (..)
, Interp (..)
, Space

-- * Sentence analysis
, asDag
, asPaths
, asPath

-- * Utilities
, toPaths
, mapL
, concatL
, concatMapL
, module Data.Either
) where

import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Control.Monad.State (evalState, put, get)
import Data.Function (on)
import Data.List (groupBy)
import Data.Char (isSpace)
import Data.Either
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import Foreign.C.String (CString)

import NLP.Morfeusz.Lock (lock)

#include "morfeusz.h"

-- | Morfeusz options
newtype MorfOption = MorfOption { unMorfOption :: CInt }
    deriving (Eq, Show)

newtype Encoding = Encoding { unEncoding :: CInt }
    deriving (Eq, Show)

newtype WhiteSpace = WhiteSpace { unWhiteSpace :: CInt }
    deriving (Eq, Show)

#{ enum MorfOption, MorfOption
 , encoding   = MORFOPT_ENCODING
 , whitespace = MORFOPT_WHITESPACE }

#{ enum Encoding, Encoding
 , utf8         = MORFEUSZ_UTF_8
 , iso8859_2    = MORFEUSZ_ISO8859_2
 , cp1250       = MORFEUSZ_CP1250
 , cp852        = MORFEUSZ_CP852 }

#{ enum WhiteSpace, WhiteSpace
 , skip_whitespace = MORFEUSZ_SKIP_WHITESPACE
 , keep_whitespace = MORFEUSZ_KEEP_WHITESPACE }

-- | Set the encoding.
setEncoding :: Encoding -> IO Bool
setEncoding enc = (1 ==) <$>
    c_morfeusz_set_option (unMorfOption encoding) (unEncoding enc)

-- | Set the Morfeusz whitespace option.
setSpace :: WhiteSpace -> IO Bool
setSpace spc = (1 ==) <$>
    c_morfeusz_set_option (unMorfOption whitespace) (unWhiteSpace spc)

-- | A directed edge with label of type @a@ between nodes of type 'Int'.
data Edge a = Edge
    { from  :: Int
    , to    :: Int
    , label :: a }
    deriving (Eq, Ord, Show, Functor)

-- | Raw morphosyntactic interpretation as presented by the Morfeusz.
data RawInterp = RawInterp
    { _orth :: T.Text
    , _base :: Maybe T.Text
    , _msd  :: Maybe T.Text }
    deriving (Eq, Ord, Show)

-- | A token with a list of recognized interpretations.  If the list of
-- interpretations is empty, the token is unknown to the Morfeusz.
data Token = Token
    { orth      :: T.Text
    , interps   :: [Interp] }
    deriving (Show)

-- | An interpretation of the word.
data Interp = Interp
    { base :: T.Text
    , msd  :: T.Text }
    deriving (Show)

-- | A space. 
type Space = T.Text

-- | We only provide the peek functionality.
instance Storable (Edge RawInterp) where
    sizeOf    _ = (#size InterpMorf)
    alignment _ = alignment (undefined :: CString)  -- or CInt ?
    peek ptr = do
        from <- getInt ((#peek InterpMorf, p) ptr)
        to <- getInt ((#peek InterpMorf, k) ptr)
        (orth, base, msd) <- if from == -1
            then return ("", Nothing, Nothing)
            else (,,)
                <$> getText ((#peek InterpMorf, forma) ptr)
                <*> getTextMaybe ((#peek InterpMorf, haslo) ptr)
                <*> getTextMaybe ((#peek InterpMorf, interp) ptr)
        return $ Edge from to (RawInterp orth base msd)
      where
        getInt = fmap fromIntegral :: IO CInt -> IO Int
        getText cStrIO = peekText =<< cStrIO
        getTextMaybe cStrIO = cStrIO >>= \cStr -> do
            if cStr == nullPtr
                then return Nothing
                else Just <$> peekText cStr
        peekText xs = T.decodeUtf8 <$> B.packCString xs

foreign import ccall unsafe "morfeusz_analyse"
    -- InterpMorf *morfeusz_analyse(char *tekst)
    c_morfeusz_analyse :: CString -> IO (Ptr (Edge RawInterp))

foreign import ccall unsafe "morfeusz_set_option"
    -- int morfeusz_set_option(int option, int value)
    c_morfeusz_set_option :: CInt -> CInt -> IO CInt

-- | A DAG with annotated edges. 
type DAG a = [Edge a]

-- | Analyse the word and output raw Morfeusz results.
analyse :: T.Text -> DAG RawInterp
analyse word = run $ \cword -> lock $ do
    _ <- setEncoding utf8
    _ <- setSpace skip_whitespace
    interp_ptr <- c_morfeusz_analyse cword
    when (interp_ptr == nullPtr) (fail $ "analyse: null pointer")
    retrieve 0 interp_ptr
  where
    run = unsafePerformIO . B.useAsCString (T.encodeUtf8 word)
    retrieve k ptr = do
        x <- peekElemOff ptr k
        if from x == -1
            then return []
            else (:) <$> return x <*> retrieve (k + 1) ptr

-- | Translate the DAG of raw Morfeusz interpretations to
-- DAG labeled with tokens.
properDAG :: DAG RawInterp -> DAG Token
properDAG dag =
    [Edge p q t | ((p, q), t) <- M.toAscList m]
  where
    m = M.fromListWith (<>) [((p, q), fromRaw r) | Edge p q r <- dag]
    fromRaw (RawInterp o (Just b) (Just m)) = Token o [Interp b m] 
    fromRaw (RawInterp o _ _)               = Token o [] 
    Token orth xs <> Token _ ys = Token orth (xs ++ ys)

-- | Analyse the input sentence as a DAG of tokens interspersed by spaces.
-- The sentence is divided on spaces first and only individual words are
-- delivererd to Morfeusz library for morphosyntactic analysis.
asDag :: T.Text -> [Either (DAG Token) Space]
asDag =
    flip evalState 0 . mapM updateIxs . map mkElem . T.groupBy cmp
  where
    cmp x y = isSpace x == isSpace y
    mkElem x
        | T.any isSpace x   = Right x
        | otherwise         = Left . properDAG . analyse $ x
    updateIxs (Right x) = return (Right x)
    updateIxs (Left xs) = Left <$> updateDAG xs
    updateDAG xs        = do
        n <- get
        let m  = maximum . map to $ xs
            ys = map (shift n) xs
        put (n + m)
        return ys
    shift k Edge{..} = Edge (from + k) (to + k) label

-- | Retrieve all paths from the root to leaves.
toPaths :: DAG a -> [[a]]
toPaths dag =
    doIt .fst . M.findMin $ m
  where
    m = M.fromListWith (++) [(from e, [e]) | e <- dag]
    doIt p = case M.lookup p m of
        Just es -> [(label e : path) | e <- es, path <- doIt (to e)]
        Nothing -> [[]]

-- | Similar to the 'asDag' function but instead of a token DAG it returns
-- all DAG paths (using the 'toPaths' function) for each word in the
-- input sentence.
asPaths :: T.Text -> [Either [[Token]] Space]
asPaths = mapL toPaths . asDag

-- | Analyse the input sentence and arbitrarily choose one path
-- from the output DAG.
asPath :: T.Text -> [Either Token Space]
asPath = 
    let hd []     = error "asPath.head: empty list"
        hd (x:xs) = x
    in  concatMapL hd . asPaths

-- | Map the function over left elements.
mapL :: (a -> a') -> [Either a b] -> [Either a' b]
mapL f =
    let g (Left x)  = Left (f x)
        g (Right y) = Right y
    in  map g

-- | Concatenate left elements.
concatL :: [Either [a] b] -> [Either a b]
concatL =
    let liftL (Left xs) = [Left x | x <- xs]
        liftL (Right y) = [Right y]
    in  concat . map liftL

-- | Map the function over left elements and concatenate results.
concatMapL :: (a -> [b]) -> [Either a c] -> [Either b c]
concatMapL f = concatL . mapL f
