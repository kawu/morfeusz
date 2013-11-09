-- | Provides a single global lock for @'IO'@ actions. Implementation borrowed
-- from the http://hackage.haskell.org/package/global-lock package.
module NLP.Morfeusz.Lock
    ( lock
    ) where

import Control.Concurrent.MVar

import NLP.Morfeusz.Lock.Internal ( get )

-- | Take the global lock for the duration of an @'IO'@ action.
--
-- Two actions executed via @'lock'@ will never run simultaneously.
lock :: IO a -> IO a
lock act = get >>= flip withMVar (const act)
