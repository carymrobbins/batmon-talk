module Batmon.TextConversion where

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.Text as Text

class TextConversion a where
  convertToText :: a -> Text
  convertFromText :: Text -> Either String a

-- | Automatic case-insensitive text conversions
newtype DefaultTextConversion a = DefaultTextConversion a
  deriving newtype (Show, Bounded, Enum)

instance (Show a, Bounded a, Enum a) => TextConversion (DefaultTextConversion a) where
  convertToText a = Text.pack $ case show a of
    "" -> ""
    c:cs -> Char.toLower c : cs

  convertFromText t =
    maybe
      (Left $ "Expected one of: " <> show ks <> "; got: " <> show t)
      Right
      (lookup (Text.toLower t) kvs)
    where
    mkAssoc a = (Text.toLower $ Text.pack $ show a, a)
    kvs = mkAssoc <$> [minBound..maxBound]
    ks = fst <$> kvs

newtype TextConversionJSON a = TextConversionJSON a
  deriving newtype (TextConversion)

instance (TextConversion a) => ToJSON (TextConversionJSON a) where
  toJSON (TextConversionJSON a) = Aeson.toJSON $ convertToText a

instance (TextConversion a) => FromJSON (TextConversionJSON a) where
  parseJSON = either fail pure . convertFromText <=< Aeson.parseJSON
