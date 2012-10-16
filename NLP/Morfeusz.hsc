{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NLP.Morfeusz
( Interp (..)
, analyse
) where

import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.Function (on)
import Data.List (groupBy)
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

-- + Funkcja analyse nie powinna być uruchamiana z dwóch wątków na raz
-- + Funckja 'analyse' powinna zwracać listę (Either Token Space)
-- - Zamienić typy ze "String" na "Text"
-- - Wynik analizy jako DAG

-- | Haskell structure for internal representation of Morfeusz interpretation.
data MorfInterp = MorfInterp
    { _beg :: Int
    , _end :: Int
    , _orth :: T.Text
    , _base :: Maybe T.Text
    , _msd :: Maybe T.Text }
    deriving (Eq, Show)

-- | A space. 
type Space = T.Text

-- | A token.
data Token
    = Tok
        { orth      :: T.Text
        , interps   :: [Interp] }
    | Unk
        { orth      :: T.Text }
    deriving (Show)

-- | An interpretation of the word.
data Interp = Interp
    { base :: T.Text
    , msd  :: T.Text }
    deriving (Show)

-- | We don't provide the poke functionality, we don't need it.
instance Storable MorfInterp where
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
        return $ MorfInterp beg end orth base msd
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
    c_morfeusz_analyse :: CString -> IO (Ptr MorfInterp)

foreign import ccall unsafe "morfeusz_set_option"
    -- int morfeusz_set_option(int option, int value)
    c_morfeusz_set_option :: CInt -> CInt -> IO CInt

-- | Get Morfeusz analysis as a list of possible interpretation sets
-- for each segment in the input word.
-- TODO: Add some info about the global lock to docs.
analyse :: T.Text -> [Either Token Space]
analyse word = run $ \cword -> lock $ do
    _ <- setEncoding utf8
    _ <- setSpace keep_whitespace
    interp_ptr <- c_morfeusz_analyse cword
    when (interp_ptr == nullPtr) (fail $ "null pointer")
    map mkTok . groupBy ((==) `on` _beg) <$> retrieve 0 interp_ptr
  where
    run = unsafePerformIO . B.useAsCString (T.encodeUtf8 word)
    retrieve k ptr = do
        x <- peekElemOff ptr k
        if _beg x == -1
            then return []
            else (:) <$> return x <*> retrieve (k + 1) ptr
    mkTok [m@(MorfInterp {_msd = Just "sp"})] = Right       $ _orth m
    mkTok [m@(MorfInterp {_msd = Nothing})]   = Left . Unk  $ _orth m
    mkTok (m : ms) = Left . Tok (_orth m) . map fromMorf $ (m : ms)
    fromMorf MorfInterp{..} = Interp
        { base = fromJust _base
        , msd  = fromJust _msd }
    fromJust (Just x) = x
    fromJust Nothing  = error "fromJust: Nothing"
