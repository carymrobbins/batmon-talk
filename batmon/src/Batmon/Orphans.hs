{-# OPTIONS_GHC -fno-warn-orphans #-}
module Batmon.Orphans where

-- Don't import Batmon.Prelude here; will cause a circular reference.
import Prelude
import Control.Compose ((:.))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import Data.Time.Zones.All (TZLabel, fromTZName, toTZName)
import GHC.Generics (Generic)
import qualified Control.Compose as Compose
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time

deriving stock instance Generic Time.LocalTime
deriving stock instance Generic Time.TimeZone
deriving stock instance Generic Time.ZonedTime

instance ToJSON TZLabel where
  toJSON = Aeson.String . Text.decodeUtf8 . toTZName

instance FromJSON TZLabel where
  parseJSON v = do
    t <- parseJSON v
    let bs = Text.encodeUtf8 t
    maybe (fail $ "Invalid TZLabel: " <> show t) pure (fromTZName bs)

instance (MonadIO f, Monad (f :. g), Applicative g)
  => MonadIO (f :. g) where
  liftIO x = Compose.O $ (pure @g) <$> (liftIO @f x)
