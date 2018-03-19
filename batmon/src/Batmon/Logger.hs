module Batmon.Logger
  ( module Batmon.Logger
  , module Control.Monad.Logger
  ) where

import Prelude
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger
import Control.Monad.Reader (MonadReader, asks)

type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | Run a LoggingT via the supplied Logger.
withLogger :: (MonadIO m) => Logger -> LoggingT m a -> m a
withLogger logger loggingT = runLoggingT loggingT logger

-- | Convert a LoggingT function to a Logger.
toLogger :: (LoggingT IO () -> IO ()) -> Logger
toLogger f loc src lvl msg =
  f $ LoggingT $ \logger -> liftIO $ logger loc src lvl msg

-- | Derive an implementation for monadLoggerLog given
-- a function which can retrieve a Logger via MonadReader.
monadLoggerLogFromReader
  :: (MonadReader r m, MonadIO m, ToLogStr msg)
  => (r -> Logger) -> Loc -> LogSource -> LogLevel -> msg -> m ()
monadLoggerLogFromReader f loc src lvl msg = do
  logger <- asks f
  liftIO $ logger loc src lvl $ toLogStr msg
