{-| These are just used by the RTS -}

module GHCJS.Prim.Internal (blockedIndefinitelyOnMVar) where

import Control.Exception

blockedIndefinitelyOnMVar :: SomeException
blockedIndefinitelyOnMVar = toException BlockedIndefinitelyOnMVar
