{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

-- | The module provides the 'analyse' wrapper function which uses the
-- Morfeusz library for morphosyntactic analysis.  The result is represented
-- as a directed acylic graph (DAG) with 'Token' labeled edges.
-- The DAG representation is needed when the input word has multiple
-- correct segmentations.
--
-- >>> :m NLP.Morfeusz
-- >>> :set -XOverloadedStrings
-- >>> mapM_ print . analyse False $ "miałem"
-- Edge {from = 0, to = 1, label = Token {orth = "mia\322", interps = [Interp {base = "mie\263", msd = "praet:sg:m1.m2.m3:imperf"}]}}
-- Edge {from = 0, to = 2, label = Token {orth = "mia\322em", interps = [Interp {base = "mia\322", msd = "subst:sg:inst:m3"}]}}
-- Edge {from = 1, to = 2, label = Token {orth = "em", interps = [Interp {base = "by\263", msd = "aglt:sg:pri:imperf:wok"}]}}
--
-- You can use the 'paths' function to extract all paths from the resultant
-- DAG and, if you are not interested in all possible segmentations, just
-- take the first of possible paths:
--
-- >>> mapM_ print . paths . analyse False $ "miałem"
-- [Token {orth = "mia\322em", interps = [Interp {base = "mia\322", msd = "subst:sg:inst:m3"}]}]
-- [Token {orth = "mia\322", interps = [Interp {base = "mie\263", msd = "praet:sg:m1.m2.m3:imperf"}]},Token {orth = "em", interps = [Interp {base = "by\263", msd = "aglt:sg:pri:imperf:wok"}]}]
-- >>> mapM_ print . head . paths . analyse False $ "miałem"
-- Token {orth = "mia\322em", interps = [Interp {base = "mia\322", msd = "subst:sg:inst:m3"}]}

module NLP.Morfeusz
(
-- * Types
  DAG
, Edge (..)
, Token (..)
, Interp (..)

-- * Sentence analysis
, KeepSpaces
, analyse

-- * Utilities
, paths
) where

import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
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

-- | Keep spaces in the analysis output.
type KeepSpaces = Bool

-- | Analyse the word and output raw Morfeusz results.
analyseRaw :: KeepSpaces -> T.Text -> DAG RawInterp
analyseRaw keepSp word = run $ \cword -> lock $ do
    _ <- setEncoding utf8
    _ <- setSpace $ if keepSp then keep_whitespace else skip_whitespace
    interp_ptr <- c_morfeusz_analyse cword
    when (interp_ptr == nullPtr) (fail $ "analyseRaw: null pointer")
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

-- | Analyse the input sentence and return the result as a DAG of tokens.
analyse :: KeepSpaces -> T.Text -> DAG Token
analyse keepSp = properDAG . analyseRaw keepSp

-- | Retrieve all paths from DAG root to leaves.
paths :: DAG a -> [[a]]
paths dag =
    doIt .fst . M.findMin $ m
  where
    m = M.fromListWith (++) [(from e, [e]) | e <- dag]
    doIt p = case M.lookup p m of
        Just es -> [(label e : path) | e <- es, path <- doIt (to e)]
        Nothing -> [[]]
