{-# LANGUAGE MagicHash, DeriveDataTypeable, CPP, JavaScriptFFI #-}

module GHCJS.Prim (JSRef(..), JSException(..), mkJSException) where

import           Data.Typeable (Typeable)
import           Unsafe.Coerce (unsafeCoerce)

import           GHC.Prim
import qualified GHC.Exception as Ex

{-
  JSRef is a boxed type that can be used as FFI
  argument or result.
-}
#ifdef ghcjs_HOST_OS
data JSRef a = JSRef ByteArray#
#else
data JSRef a = JSRef Addr#
#endif

{-
  When a JavaScript exception is raised inside
  a safe or interruptible foreign call, it is converted
  to a JSException
 -}
data JSException = JSException (JSRef ()) String
  deriving (Typeable)

instance Ex.Exception JSException

instance Show JSException where
  show (JSException _ xs) = "JavaScript exception: " ++ xs

foreign import javascript unsafe "h$toHsString(\"\" + $1)"
   js_toString :: JSRef a -> IO Int

mkJSException :: JSRef a -> IO JSException
mkJSException ref = do
    xs <- js_toString ref
    return (JSException (unsafeCoerce ref) (unsafeCoerce xs))

