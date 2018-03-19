module Batmon.Google.Calendar.Response where

import Batmon.Prelude
import Batmon.Google.Calendar (Calendar, CalendarEvent, ClientFailure(ClientFailure), Keyed)
import Control.Exception (Exception)
import Control.Monad.Except (MonadError(throwError, catchError))
import Data.Aeson (FromJSON)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable, typeRep)
import Data.UUID (UUID)
import Data.Word (Word8)
import Text.Read (readMaybe)
import qualified Batmon.Google.Calendar as ClientFailure (ClientFailure(..))
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString.Lazy as Atto
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID

data EncodedResponse = EncodedResponse
  { status :: Int
  , headers :: [(CI ByteString, ByteString)]
  , body :: ByteString
  } deriving stock (Show, Eq)

isOk :: Int -> Bool
isOk s = s `div` 200 == 1

isNotFound :: Int -> Bool
isNotFound = (== 404)

newtype DecodeResult a = DecodeResult
  { unDecodeResult :: Either DecodeFailure a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError DecodeFailure
    )

newtype DecodeFailure = DecodeFailure String
  deriving stock (Show, Eq, Typeable)
  deriving newtype (Semigroup)
  deriving anyclass (Exception)

decodeFailure :: String -> DecodeResult a
decodeFailure = throwError . DecodeFailure

typeName :: forall a s. (Typeable a, IsString s) => s
typeName = fromString $ show $ typeRep (Proxy @a)

responseFailure :: forall a. (Typeable a) => Int -> DecodeResult a
responseFailure s =
  decodeFailure $
    "Response failed with status code " <> show s
      <> "; failed to decode response of type " <> (typeName @a)

checkingStatus
  :: (Typeable a) => EncodedResponse -> DecodeResult a -> DecodeResult a
checkingStatus r ifOk =
  if (isOk $ status r) then
    ifOk
  else
    responseFailure $ status r

class (Typeable a) => Decoder a where
  decode :: EncodedResponse -> DecodeResult a

instance Decoder () where
  decode = flip checkingStatus $ pure ()

newtype JSONEncoded a = JSONEncoded { unJSONEncoded :: a }

instance (Typeable a, FromJSON a) => Decoder (JSONEncoded a) where
  decode r =
    checkingStatus r $ DecodeResult $ do
      bimap
        (\s -> DecodeFailure $ "Failed to decode " <> typeName @a <> " JSON: " <> s)
        JSONEncoded
        (Aeson.eitherDecodeStrict $ body r)

deriving via JSONEncoded (Keyed Calendar) instance Decoder (Keyed Calendar)
deriving via JSONEncoded (Keyed CalendarEvent) instance Decoder (Keyed CalendarEvent)

instance (Decoder a) => Decoder (Maybe a) where
  decode r =
    if isNotFound $ status r then
      pure Nothing
    else
      Just <$> (decode @a r)

instance (Decoder a) => Decoder (Either ClientFailure a) where
  decode r =
    catchError (Right <$> decode @a r) $ \e ->
      if isOk $ status r then
        throwError e
      else
        pure $ Left ClientFailure
          { ClientFailure.status = status r
          , ClientFailure.message = Text.decodeUtf8 $ body r
          }

newtype ParseFailure = ParseFailure { unParseFailure :: String }
  deriving stock (Eq, Show, Typeable)
  deriving anyclass (Exception)


-- | Parse EncodedResponse values and their associated UUIDs from
-- a multipart/mixed body.
-- Unfortunately, the 'multipart' package doesn't seem to parse these
-- bodies properly, so we'll do it by hand with attoparsec.
parse :: L.ByteString -> Either ParseFailure (Map UUID EncodedResponse)
parse inputBS = case Atto.parse parser inputBS of
  Atto.Done _ xs -> Right $ Map.fromList xs
  Atto.Fail _ contexts msg ->
    Left $ ParseFailure $
      "Failed to parse response: " <> msg <> "\n"
        <> List.intercalate "\n" contexts
  where
  (<?>) = (Atto.<?>)

  parser :: Atto.Parser [(UUID, EncodedResponse)]
  parser = do
    boundary <- detectBoundary
    let pBoundary = "--" *> Atto.string boundary
    res <- Atto.sepBy1 encodedResp (pBoundary <?> "boundary sep")
    void $ pBoundary <?> "boundary end"
    void $ Atto.string "--" <?> "-- end"
    crlf
    Atto.endOfInput
    pure res

  pUntilCR1 :: Atto.Parser ByteString
  pUntilCR1 = Atto.takeWhile1 (/= unsafeOrd8 '\r') <?> "many1 /= CR"

  detectBoundary :: Atto.Parser ByteString
  detectBoundary = (<?> "detect boundary") $ do
    void $ Atto.string "--" <?> "-- start"
    pUntilCR1

  isNum :: Word8 -> Bool
  isNum w = w >= unsafeOrd8 '0' && w <= unsafeOrd8 '9'

  isAlphaNum :: Word8 -> Bool
  isAlphaNum w =
       isNum w
    || (w >= unsafeOrd8 'A' && w <= unsafeOrd8 'Z')
    || (w >= unsafeOrd8 'a' && w <= unsafeOrd8 'z')

  crlf = void (Atto.string "\r\n") <?> "crlf"

  crlf2 = void (crlf *> crlf) <?> "crlf2"

  encodedResp :: Atto.Parser (UUID, EncodedResponse)
  encodedResp = do
    crlf
    uuid <- contentIdFromHeaders
    crlf2
    status <- pStatus
    crlf
    headers <- pHeaders
    crlf2
    body <- Atto.takeWhile (/= unsafeOrd8 '\r')
    crlf
    pure (uuid, EncodedResponse {status, headers, body})

  contentIdFromHeaders :: Atto.Parser UUID
  contentIdFromHeaders = (<?> "content-id from headers") $ do
    hs <- pHeaders
    case List.find ((== "content-id") . fst) hs of
      Just (_, v) -> extractContentId v
      Nothing -> fail "Failed to find content-id header"

  extractContentId :: ByteString -> Atto.Parser UUID
  extractContentId = pUUID . C8.take 36 . C8.drop 10

  pUUID :: ByteString -> Atto.Parser UUID
  pUUID s =
    maybe
      (fail $ "Invalid UUID: " <> show s)
      pure
      (UUID.fromASCIIBytes s)

  pDigit = Atto.satisfy isNum <?> "digit"

  pStatus :: Atto.Parser Int
  pStatus = do
    void $ Atto.string "HTTP/1.1 " <?> "HTTP/1.1 "
    code <- pInt =<< (B.pack <$> Atto.count 3 pDigit <?> "3 digits")
    void $ Atto.skipWhile (/= unsafeOrd8 '\r')
    pure code

  pInt :: ByteString -> Atto.Parser Int
  pInt s = case readMaybe @Int (C8.unpack s) of
    Just n -> pure n
    Nothing -> fail $ "Invalid int: " <> show s

  pHeaders :: Atto.Parser [(CI ByteString, ByteString)]
  pHeaders = Atto.sepBy1 header crlf <?> "headers"

  header :: Atto.Parser (CI ByteString, ByteString)
  header = do
    name <- headerName
    void $ Atto.word8 (unsafeOrd8 ':') <?> "header sep"
    void $ Atto.takeWhile (== unsafeOrd8 ' ') <?> "header whitespace"
    val <- pUntilCR1 <?> "header value"
    pure (CI.mk name, val)

  headerName :: Atto.Parser ByteString
  headerName =  (<?> "header name") $
    Atto.takeWhile1 $ \w ->
         isAlphaNum w
      || w == unsafeOrd8 '_'
      || w == unsafeOrd8 '-'

unsafeOrd8 :: Char -> Word8
unsafeOrd8 = fromIntegral . Char.ord
