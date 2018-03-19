{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Batmon.Google.Calendar.ResponseSpec where

import Batmon.Prelude
import Control.Exception (throw)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import Data.Bifunctor (bimap)
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Text (Text)
import Test.Hspec
import qualified Batmon.Google.Calendar.Response as Response
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import qualified Data.List as List
import qualified Data.Text.Encoding as Text
import qualified System.FilePath as FilePath

spec :: Spec
spec = do
  describe "parse" $ do
    let fixtures =
          List.groupBy (on (==) (dirNames . fst)) $
            List.sortBy (comparing fst) $
              $(embedDir =<< makeRelativeToProject "testdata/response")

    for_ fixtures $ \files -> do
      let [name] = dirNames $ fst $ head files
      it ("should parse fixture " <> name) $ do
        let Just responseTxt = lookup (name FilePath.</> "response.txt") files
        let Just expectedJson = lookup (name FilePath.</> "expected.json") files
        case Aeson.eitherDecodeStrict' expectedJson of
          Left msg -> throw $ userError msg
          Right expected -> do
            Response.parse (L.fromStrict responseTxt) `shouldBe` Right expected
  where
  dirNames :: FilePath -> [FilePath]
  dirNames = \case
    "" -> throw $ userError "Empty file path!"
    s -> init $ FilePath.splitDirectories s

-- | Orphan so we can easily load test fixtures.
instance FromJSON Response.EncodedResponse where
  parseJSON = Aeson.withObject "EncodedResponse" $ \o -> do
    status :: Int <- o Aeson..: "status"
    headers :: [(Text, Text)] <- o Aeson..: "headers"
    body :: Text <- o Aeson..: "body"
    pure Response.EncodedResponse
      { Response.status = status
      , Response.headers =
          bimap (CI.mk . Text.encodeUtf8) Text.encodeUtf8 <$> headers
      , Response.body = Text.encodeUtf8 body
      }

-- | Orphan so we can easily save test fixtures.
instance ToJSON Response.EncodedResponse where
  toJSON Response.EncodedResponse{..} =
    Aeson.object
      [ "status" Aeson..= toJSON status
      , "headers" Aeson..= toJSON
          (bimap (Text.decodeUtf8 . CI.original) Text.decodeUtf8 <$> headers)
      , "body" Aeson..= toJSON (Text.decodeUtf8 body)
      ]
