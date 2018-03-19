module Batmon.JSON where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic, Rep)
import Prelude
import qualified Data.Aeson as Aeson

newtype DefaultJSON a = DefaultJSON a

defaultJSONOptions :: Aeson.Options
defaultJSONOptions = Aeson.defaultOptions
  { Aeson.omitNothingFields = True }

instance
  ( Generic a
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  ) => FromJSON (DefaultJSON a)
  where
  parseJSON v = DefaultJSON <$> (Aeson.genericParseJSON @a defaultJSONOptions v)

instance
  ( Generic a
  , Aeson.GToJSON Aeson.Zero (Rep a)
  , Aeson.GToEncoding Aeson.Zero (Rep a)
  ) => ToJSON (DefaultJSON a)
  where
  toJSON (DefaultJSON a) = Aeson.genericToJSON defaultJSONOptions a
  toEncoding (DefaultJSON a) = Aeson.genericToEncoding defaultJSONOptions a

