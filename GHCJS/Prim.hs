{-# LANGUAGE MagicHash #-}

module GHCJS.Prim (JSRef(..)) where

import GHC.Prim

{-
  JSRef is a boxed type that can be used as FFI
  argument or result.
-}
data JSRef a = JSRef ByteArray#

