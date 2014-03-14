{-| These are just used by the RTS -}

module GHCJS.Prim.Internal (blockedIndefinitelyOnMVar) where

import Control.Exception

import GHCJS.Prim

wouldBlock :: String -> SomeException
wouldBlock = toException . WouldBlockException

blockedIndefinitelyOnMVar :: SomeException
blockedIndefinitelyOnMVar = toException BlockedIndefinitelyOnMVar

blockedIndefinitelyOnSTM :: SomeException
blockedIndefinitelyOnSTM = toException BlockedIndefinitelyOnSTM
