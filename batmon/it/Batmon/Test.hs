module Batmon.Test
  ( module Batmon.Test
  , module X
  ) where

-- Re-exports
import Batmon.Logger as X
import Batmon.Prelude as X
import Test.Hspec as X

import Control.Exception (PatternMatchFail, evaluate)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import GHC.Stack (HasCallStack, callStack, getCallStack, SrcLoc)
import qualified Batmon.Google.Calendar as G
import qualified Batmon.Google.Calendar.Batch as G
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Client.TLS as HTTPClientTLS
import qualified System.Environment as Environment
import qualified System.IO.Unsafe
import qualified Test.HUnit.Lang as HUnit

logger :: Logger
logger = toLogger $ case envLog of
  NoLogging -> const $ pure ()
  StdErrLogging -> runStderrLoggingT
  FileLogging -> runFileLoggingT "batmon/log/batmon.test.log"

debug :: String -> IO ()
debug = withLogger logger . $logDebug . Text.pack

newGClientWithCounter :: IO (G.Client IO, IORef Int)
newGClientWithCounter = do
  counter <- liftIO $ newIORef (0 :: Int)
  manager <- liftIO $ HTTPClient.newManager HTTPClientTLS.tlsManagerSettings
  let httpHandler =
        G.httpHandlerWithCounter counter $
          G.mkBatchedHttpHandler manager logger
  pure (G.mkBatchedClient httpHandler logger, counter)

newEventSummary :: IO Text
newEventSummary = ("ClientITSpec" <>) . UUID.toText <$> UUID.nextRandom

-- | Obtains a global GOOGLE_ACCESS_TOKEN for testing.
-- You can get an access token easily from the OAuth2 playground
-- https://developers.google.com/oauthplayground
{-# NOINLINE envGoogleAccessToken #-}
envGoogleAccessToken :: G.AccessToken
envGoogleAccessToken = System.IO.Unsafe.unsafePerformIO $ do
  Environment.lookupEnv "GOOGLE_ACCESS_TOKEN" <&> \case
    Nothing -> throw $ userError "GOOGLE_ACCESS_TOKEN env var is required but not set!"
    Just s -> G.AccessToken $ Text.pack s

envGoogleEmail :: Text
envGoogleEmail = System.IO.Unsafe.unsafePerformIO $ do
  Environment.lookupEnv "GOOGLE_EMAIL" <&> \case
    Nothing -> throw $ userError "GOOGLE_EMAIL env var is required but not set!"
    Just s -> Text.pack s

envLog :: LoggerType
envLog = System.IO.Unsafe.unsafePerformIO $ do
  Environment.lookupEnv "LOG" <&> \case
    Nothing       -> NoLogging
    Just "stderr" -> StdErrLogging
    Just "file"   -> FileLogging
    Just s -> throw $ userError $ "Unknown LOGGER value: " <> show s

data LoggerType = NoLogging | StdErrLogging | FileLogging

-- | Read an IORef Int but set it to zero afterwards, atomically.
readAndZeroIORef' :: IORef Int -> IO Int
readAndZeroIORef' = flip atomicModifyIORef' (\n -> (0, n))

expectJust :: (HasCallStack) => Maybe a -> IO a
expectJust = \case
  Just b -> pure b
  Nothing -> throwHUnit $ "Expected Just, got: Nothing"

expectRight :: (HasCallStack, Show a, Show b) => Either a b -> IO b
expectRight = \case
  Right b -> pure b
  x@(Left _) -> throwHUnit $ "Expected Right, got: " <> show x

-- | Assert that a pattern match succeeds; may require -fno-warn-incomplete-patterns
expectPattern :: (HasCallStack, Show a) => (a -> b) -> a -> IO b
expectPattern f a =
  try (evaluate $ f a) >>= \case
    Right b -> pure b
    Left (_ :: PatternMatchFail) ->
      throwHUnit $ "Pattern match failed, value was: " <> show a

-- | Same as 'expectPattern' but with its arguments flipped.
shouldMatchPattern :: (HasCallStack, Show a) => a -> (a -> b) -> IO b
shouldMatchPattern = flip expectPattern

-- | Same as 'shouldMatchPattern' but with the return type specialized as unit.
-- Useful for pattern matching on GADTs.
shouldMatchPattern_ :: (HasCallStack, Show a) => a -> (a -> ()) -> IO ()
shouldMatchPattern_ = shouldMatchPattern

-- | Obtain the source location given a reverse call stack index.
callStackLoc :: (HasCallStack) => Int -> Maybe SrcLoc
callStackLoc index = fmap snd $ listToMaybe $ drop index $ reverse $ getCallStack callStack

-- | Throw an test failed exception, defaulting the source location to the caller's caller.
throwHUnit :: (HasCallStack) => String -> IO a
throwHUnit = throwHUnitWithLoc 0

-- | Throw a test failure exception with source location determined by the supplied reverse call stack index.
throwHUnitWithLoc :: (HasCallStack) => Int -> String -> IO a
throwHUnitWithLoc index msg = throwIO $ HUnit.HUnitFailure (callStackLoc index) $ HUnit.Reason msg
