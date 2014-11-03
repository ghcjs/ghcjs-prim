{-# LANGUAGE CPP, LambdaCase, BangPatterns, MagicHash, TupleSections, ScopedTypeVariables, DeriveDataTypeable #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE JavaScriptFFI #-}
#endif

{- |
     Evaluate Template Haskell splices on node.js
 -}

module GHCJS.Prim.TH.Eval (
#ifdef ghcjs_HOST_OS
         runTHServer
#endif
       ) where

#ifdef ghcjs_HOST_OS

import           GHCJS.Prim.TH.Serialized
import           GHCJS.Prim.TH.Types

import           Control.Applicative
import qualified Control.Exception        as E
import           Control.Monad

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Unsafe   as BU
import           Data.Data
import           Data.Dynamic
import           Data.Int
import           Data.IORef
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Maybe
import           Data.Monoid              ((<>))
import           Data.Typeable
import           Data.Typeable.Internal
import           Data.Word

import           Foreign.C
import           Foreign.Ptr

import           GHC.Prim
import           GHC.Desugar

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

import           System.IO

import           Unsafe.Coerce

data QState = QState { qsMap        :: Map TypeRep Dynamic    -- ^ persistent data between splices in a module
                     , qsFinalizers :: [TH.Q ()]              -- ^ registered finalizers (in reverse order)
                     , qsMessages   :: IORef [(Bool, String)] -- ^ messages reported
                     , qsLocation   :: Maybe TH.Loc           -- ^ location for current splice, if any
                     }
instance Show QState where show _ = "<QState>"

initQState :: IORef [(Bool, String)] -> QState
initQState msgs = QState M.empty [] msgs Nothing

runModFinalizers :: GHCJSQ ()
runModFinalizers = go =<< getState
  where
    go s | (f:ff) <- qsFinalizers s =
      putState (s { qsFinalizers = ff}) >> TH.runQ f >> getState >>= go
    go _ = return ()

data GHCJSQ a = GHCJSQ { runGHCJSQ :: QState -> IO (a, QState) }

data GHCJSQException = GHCJSQException QState String
  deriving (Show, Typeable)

instance E.Exception GHCJSQException

instance Functor GHCJSQ where
  fmap f (GHCJSQ s) = GHCJSQ $ fmap (\(x,s') -> (f x,s')) . s

instance Applicative GHCJSQ where
  f <*> a = GHCJSQ $ \s ->
    do (f',s')  <- runGHCJSQ f s
       (a',s'') <- runGHCJSQ a s'
       return (f' a', s'')
  pure x = GHCJSQ (\s -> return (x,s))

instance Monad GHCJSQ where
  m >>= f = GHCJSQ $ \s ->
    do (m', s')  <- runGHCJSQ m s
       (a,  s'') <- runGHCJSQ (f m') s'
       return (a, s'')
  return    = pure
  fail err  = GHCJSQ $ \s -> E.throw (GHCJSQException s err)

getState :: GHCJSQ QState
getState = GHCJSQ $ \s -> return (s,s)

putState :: QState -> GHCJSQ ()
putState s = GHCJSQ $ \_ -> return ((),s)

noLoc :: TH.Loc
noLoc = TH.Loc "<no file>" "<no package>" "<no module>" (0,0) (0,0)

instance TH.Quasi GHCJSQ where
  qNewName str = do
    NewName' name <- TH.qRunIO (sendRequest $ NewName str)
    return name
  qReport isError msg = getState >>=
    TH.qRunIO . flip modifyIORef ((isError,msg):) . qsMessages
  qRecover (GHCJSQ h) (GHCJSQ a) = GHCJSQ $ \s ->
    -- discard error messages on recovery
    a s `E.catch` \(GHCJSQException s' _) ->
      TH.qRunIO (modifyIORef (qsMessages s') (filter (not . fst))) >> h s'
  qLookupName isType occ = do
    LookupName' name <- TH.qRunIO (sendRequest $ LookupName isType occ)
    return name
  qReify name = do
    Reify' info <- TH.qRunIO (sendRequest $ Reify name)
    return info
  qReifyInstances name tys = do
    ReifyInstances' decls <- TH.qRunIO (sendRequest $ ReifyInstances name tys)
    return decls
  qReifyRoles name = do
    ReifyRoles' roles <- TH.qRunIO (sendRequest $ ReifyRoles name)
    return roles
  qReifyAnnotations lookup = do
    ReifyAnnotations' payloads <- TH.qRunIO (sendRequest $ ReifyAnnotations lookup)
    return (convertAnnPayloads payloads)
  qReifyModule m = do
    ReifyModule' mi <- TH.qRunIO (sendRequest $ ReifyModule m)
    return mi
  qLocation = fromMaybe noLoc . qsLocation <$> getState
  qRunIO m = GHCJSQ $ \s -> fmap (,s) m
  qAddDependentFile file = do
    AddDependentFile' <- TH.qRunIO (sendRequest $ AddDependentFile file)
    return ()
  qAddTopDecls decls = do
    AddTopDecls' <- TH.qRunIO (sendRequest $ AddTopDecls decls)
    return ()
  qAddModFinalizer fin = GHCJSQ $ \s ->
    return ((), s { qsFinalizers = fin : qsFinalizers s })
  qGetQ = GHCJSQ $ \s ->
    let lookup :: forall a. Typeable a => Map TypeRep Dynamic -> Maybe a
        lookup m = fromDynamic =<< M.lookup (typeOf (undefined::a)) m
    in return (lookup (qsMap s), s)
  qPutQ k = GHCJSQ $ \s ->
    return ((), s { qsMap = M.insert (typeOf k) (toDyn k) (qsMap s) })

makeAnnPayload :: forall a. Data a => a -> ByteString
makeAnnPayload x =
  let TypeRep (Fingerprint w1 w2) _ _ = typeOf (undefined :: a)
      fp = runPut (putWord64be w1 >> putWord64be w2)
  in  BL.toStrict $ fp <> BL.pack (serializeWithData x)

convertAnnPayloads :: forall a. Data a => [ByteString] -> [a]
convertAnnPayloads bs = catMaybes (map convert bs)
  where
    TypeRep (Fingerprint w1 w2) _ _ = typeOf (undefined :: a)
    getFp b = runGet ((,) <$> getWord64be <*> getWord64be) $ BL.fromStrict (B.take 16 b)
    convert b | (bw1,bw2) <- getFp b, bw1 == w1, bw2 == w2 =
                  Just (deserializeWithData . B.unpack . B.drop 16 $ b)
              | otherwise = Nothing

-- | the Template Haskell server
runTHServer :: IO ()
runTHServer = do
  msgs <- newIORef []
  void (runGHCJSQ server (initQState msgs)) `E.catches`
    [ E.Handler $ \(GHCJSQException _ msg) -> sendReportedMessages' msgs >> void (sendRequest $ QFail msg)
    , E.Handler $ \(E.SomeException e)     -> sendReportedMessages' msgs >> void (sendRequest $ QException (show e))
    ]
  where
    server = TH.qRunIO awaitMessage >>= \case
      RunTH t code loc -> do
        a <- TH.qRunIO (loadCode code)
        runTH t a loc
        sendReportedMessages
        server
      FinishTH -> do
        runModFinalizers
        sendReportedMessages
        TH.qRunIO $ sendResult FinishTH'
      _ -> error "runTHServer: unexpected message type"

{-# NOINLINE runTH #-}
runTH :: THResultType -> Any -> Maybe TH.Loc -> GHCJSQ ()
runTH rt obj = \mb_loc -> obj `seq` do
  s0 <- getState
  putState $ s0 { qsLocation = mb_loc }
  res <- case rt of
           THExp        -> runTHCode (unsafeCoerce obj :: TH.Q TH.Exp)
           THPat        -> runTHCode (unsafeCoerce obj :: TH.Q TH.Pat)
           THType       -> runTHCode (unsafeCoerce obj :: TH.Q TH.Type)
           THDec        -> runTHCode (unsafeCoerce obj :: TH.Q [TH.Dec])
           THAnnWrapper -> case unsafeCoerce obj of
                             AnnotationWrapper x -> return (makeAnnPayload x)
  sendReportedMessages
  s1 <- getState
  TH.qRunIO (sendResult $ RunTH' res)
  putState $ s1 { qsLocation = Nothing }

{-# NOINLINE runTHCode #-}
runTHCode :: Binary a => TH.Q a -> GHCJSQ ByteString
runTHCode c = BL.toStrict . runPut . put <$> TH.runQ c

{-# NOINLINE loadCode #-}
loadCode :: ByteString -> IO Any
loadCode bs = do
  p <- fromBs bs
  unsafeCoerce <$> js_loadCode p (B.length bs)

sendReportedMessages :: GHCJSQ ()
sendReportedMessages =
  getState >>= TH.qRunIO . sendReportedMessages' . qsMessages

sendReportedMessages' :: IORef [(Bool,String)] -> IO ()
sendReportedMessages' r =
  let report (isError, msg) = do
        Report' <- TH.qRunIO (sendRequest $ Report isError msg)
        return ()
  in  atomicModifyIORef r (\msgs -> ([], msgs)) >>= mapM_ report . reverse

awaitMessage :: IO Message
awaitMessage = fmap (runGet get . BL.fromStrict) . toBs =<< js_awaitMessage

-- | send result back
sendResult :: Message -> IO ()
sendResult msg = do
  let bs = BL.toStrict $ runPut (put msg)
  p <- fromBs bs
  js_sendMessage p (B.length bs)

-- | send a request and wait for the response
sendRequest :: Message -> IO Message
sendRequest msg = do
  let bs = BL.toStrict $ runPut (put msg)
  p <- fromBs bs
  fmap (runGet get . BL.fromStrict) . toBs =<< js_sendRequest p (B.length bs)

foreign import javascript interruptible "h$TH.sendRequest($1_1,$1_2,$2,$c);"
  js_sendRequest :: Ptr Word8 -> Int -> IO (Ptr Word8)

foreign import javascript interruptible "h$TH.sendMessage($1_1,$1_2,$2,0,$c);"
  js_sendMessage :: Ptr Word8 -> Int -> IO ()

foreign import javascript interruptible "h$TH.awaitMessage(0,$c);"
  js_awaitMessage :: IO (Ptr Word8)

foreign import javascript unsafe "h$TH.bufSize($1_1, $1_2)"
  js_bufSize :: Ptr Word8 -> IO Int

-- | actually returns the heap object to be evaluated
foreign import javascript unsafe "h$TH.loadCode($1_1,$1_2,$2)"
  js_loadCode :: Ptr Word8 -> Int -> IO Double

-- | only safe in JS
fromBs :: ByteString -> IO (Ptr Word8)
fromBs bs = BU.unsafeUseAsCString bs (return . castPtr)

-- | build a ByteString that uses the whole buffer, only works in JS
toBs :: Ptr Word8 -> IO ByteString
toBs p = do
  l <- js_bufSize p
  BU.unsafePackCStringLen (castPtr p, l)

#endif

