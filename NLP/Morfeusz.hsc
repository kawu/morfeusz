{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module NLP.Morfeusz
( asDag
, rmSpaces
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

setEncoding :: Encoding -> IO Bool
setEncoding enc = (1 ==) <$>
    c_morfeusz_set_option (unMorfOption encoding) (unEncoding enc)

setSpace :: WhiteSpace -> IO Bool
setSpace spc = (1 ==) <$>
    c_morfeusz_set_option (unMorfOption whitespace) (unWhiteSpace spc)

-- * Podział na słowa na poziomie Haskella.
-- * Morfeusza uruchamiamy na spójnym fragmencie tekstu bez spacji. 
-- * Na takim też fragmencie otrzymujemy DAG.

-- | A directed edge with label of type @a@ between nodes of type 'Int'.
-- TODO: Change beg -> from, end -> to.
data Edge a = Edge
    { beg :: Int
    , end :: Int
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

-- expand :: Interp -> [Interp]

-- | We don't provide the poke functionality, we don't need it.
instance Storable (Edge RawInterp) where
    sizeOf    _ = (#size InterpMorf)
    alignment _ = alignment (undefined :: CString)  -- or CInt ?
    peek ptr = do
        beg <- getInt ((#peek InterpMorf, p) ptr)
        end <- getInt ((#peek InterpMorf, k) ptr)
        (orth, base, msd) <- if beg == -1
            then return ("", Nothing, Nothing)
            else (,,)
                <$> getText ((#peek InterpMorf, forma) ptr)
                <*> getTextMaybe ((#peek InterpMorf, haslo) ptr)
                <*> getTextMaybe ((#peek InterpMorf, interp) ptr)
        return $ Edge beg end (RawInterp orth base msd)
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
    _ <- setSpace keep_whitespace
    interp_ptr <- c_morfeusz_analyse cword
    when (interp_ptr == nullPtr) (fail $ "analyse: null pointer")
    retrieve 0 interp_ptr
  where
    run = unsafePerformIO . B.useAsCString (T.encodeUtf8 word)
    retrieve k ptr = do
        x <- peekElemOff ptr k
        if beg x == -1
            then return []
            else (:) <$> return x <*> retrieve (k + 1) ptr

properDAG :: DAG RawInterp -> DAG Token
properDAG dag =
    [Edge p q t | ((p, q), t) <- M.toAscList m]
  where
    m = M.fromListWith (<>) [((p, q), fromRaw r) | Edge p q r <- dag]
    fromRaw (RawInterp o (Just b) (Just m)) = Token o [Interp b m] 
    fromRaw (RawInterp o _ _)               = Token o [] 
    Token orth xs <> Token _ ys = Token orth (xs ++ ys)

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
        let m  = maximum . map end $ xs
            ys = map (shift n) xs
        put (n + m)
        return ys
    shift k Edge{..} = Edge (beg + k) (end + k) label

rmSpaces :: [Either [a] b] -> [a]
rmSpaces = concat . lefts

-- -- | Get Morfeusz analysis as a list of possible interpretation sets
-- -- for each segment in the input word.
-- -- TODO: Add some info about the global lock to docs.
-- analyse :: T.Text -> [Either Token Space]
-- analyse word = run $ \cword -> lock $ do
--     _ <- setEncoding utf8
--     _ <- setSpace keep_whitespace
--     interp_ptr <- c_morfeusz_analyse cword
--     when (interp_ptr == nullPtr) (fail $ "null pointer")
--     map mkTok . groupBy ((==) `on` _beg) <$> retrieve 0 interp_ptr
--   where
--     run = unsafePerformIO . B.useAsCString (T.encodeUtf8 word)
--     retrieve k ptr = do
--         x <- peekElemOff ptr k
--         if _beg x == -1
--             then return []
--             else (:) <$> return x <*> retrieve (k + 1) ptr
--     mkTok [m@(MorfInterp {_msd = Just "sp"})] = Right       $ _orth m
--     mkTok [m@(MorfInterp {_msd = Nothing})]   = Left . Unk  $ _orth m
--     mkTok (m : ms) = Left . Tok (_orth m) . map fromMorf $ (m : ms)
--     fromMorf MorfInterp{..} = Interp
--         { base = fromJust _base
--         , msd  = fromJust _msd }
--     fromJust (Just x) = x
--     fromJust Nothing  = error "fromJust: Nothing"
