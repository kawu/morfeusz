{-# LANGUAGE ForeignFunctionInterface #-}

module NLP.Morfeusz
( analyse
, Interp (..) 
, setEncoding
, utf8
, iso8859_2
, cp1250
, cp852
, skip_whitespace
, keep_whitespace
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.Function (on)
import Data.List (groupBy)

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

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

-- | Haskell structure for internal representation of Morfeusz interpretation.
data MorfInterp = MorfInterp
    { _start :: Int
    , _end :: Int
    , _token :: String
    , _lemma :: Maybe String
    , _msd :: Maybe String }
    deriving (Eq, Show)

data Interp = Interp
    { token :: String
    , lemma :: Maybe String
    , msd   :: Maybe String }

morf2interp (MorfInterp _ _ token lemma msd) = Interp token lemma msd

instance Storable MorfInterp where
    sizeOf    _ = (#size InterpMorf)
    alignment _ = alignment (undefined :: CString)  -- or CInt ?
    peek ptr = do
        start <- getInt ((#peek InterpMorf, p) ptr)
        end <- getInt ((#peek InterpMorf, k) ptr)
        (token, lemma, msd) <- if start == -1
            then return ("", Nothing, Nothing)
            else (,,)
                <$> getString ((#peek InterpMorf, forma) ptr)
                <*> getStringMaybe ((#peek InterpMorf, haslo) ptr)
                <*> getStringMaybe ((#peek InterpMorf, interp) ptr)
        return $ MorfInterp start end token lemma msd
      where
        getInt = fmap fromIntegral :: IO CInt -> IO Int
        getString cStrIO = peekCString =<< cStrIO
        getStringMaybe cStrIO = cStrIO >>= \cStr -> do
            if cStr == nullPtr
                then return Nothing
                else Just <$> peekCString cStr

-- | Warning: is the C function reentrant ?  If not, API need to be changed ?

foreign import ccall unsafe "morfeusz_analyse"
    -- InterpMorf *morfeusz_analyse(char *tekst)
    c_morfeusz_analyse :: CString -> IO (Ptr MorfInterp)

foreign import ccall unsafe "morfeusz_set_option"
    -- int morfeusz_set_option(int option, int value)
    c_morfeusz_set_option :: CInt -> CInt -> IO CInt

-- | Get Morfeusz analysis as a list of possible interpretation sets
-- for each segment in the input word.
analyse :: String -> IO [[Interp]]
analyse word = withCString word $ \cword -> do
    interp_ptr <- c_morfeusz_analyse cword
    when (interp_ptr == nullPtr) (fail $ "null pointer")
    mkInterps <$> retrieve 0 interp_ptr
  where
    mkInterps = map (map morf2interp) . groupBy ((==) `on` _start)
    retrieve k ptr = do
        x <- peekElemOff ptr k
        if _start x == -1
            then return []
            else (:) <$> return x <*> retrieve (k + 1) ptr

setEncoding :: Encoding -> IO Bool
setEncoding choice = (1 ==) <$>
    c_morfeusz_set_option (unMorfOption encoding) (unEncoding choice)
