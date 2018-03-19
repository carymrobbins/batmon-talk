module Batmon.Google.Calendar.Batch where

import Batmon.Free
import Batmon.Google.Calendar
import Batmon.Google.Calendar.Response (EncodedResponse)
import Batmon.Logger
import Batmon.Prelude
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT(ReaderT, runReaderT))
import Control.Natural (type (~>))
import Data.ByteString (ByteString)
import Data.Coerce (Coercible, coerce)
import Data.IORef (IORef, modifyIORef')
import Data.Some (Some(This))
import qualified Batmon.Google.Calendar.Response as Response
import qualified Control.Applicative.Free as FreeAp
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.List.Split as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Types as HTTPTypes

newtype HttpHandler f = HttpHandler
  { runHttpHandler :: RequestPayload -> f BatchResponse }

newtype BatchResponse = BatchResponse L.ByteString

-- | Produces an 'HttpHandler' which will throw an exception for non-2xx
-- status codes.
mkBatchedHttpHandler
  :: (MonadIO m, MonadThrow m)
  => HTTPClient.Manager
  -> Logger
  -> HttpHandler m
mkBatchedHttpHandler mgr logger =
  HttpHandler $ \payload@RequestPayload{..} -> do
    let body = renderRequestPayload payload
    withLogger logger $
      $logDebug $ "Sending batch to google:\n" <> maskBearer body
    req <- (HTTPClient.parseUrlThrow url) <&> \r ->
              r { HTTPClient.method = HTTPTypes.methodPost
                , HTTPClient.requestHeaders =
                    [ (HTTPTypes.hContentType,
                        "multipart/mixed; boundary=\"" <> boundary <> "\"")
                    ]
                , HTTPClient.requestBody = HTTPClient.RequestBodyBS body
                }
    liftIO $ BatchResponse . HTTPClient.responseBody
      <$> HTTPClient.httpLbs req mgr
    where
    url = "https://www.googleapis.com/batch/calendar/v3"

-- | Masks request bodies' auth token for safer logging.
maskBearer :: ByteString -> Text
maskBearer s =
  Text.intercalate "\n" $ go <$> Text.splitOn "\n" (Text.decodeUtf8 s)
  where
  go line =
    if "Authorization: Bearer " `Text.isPrefixOf` line then
      "Authorization: Bearer ***********************"
    else
      line

-- | Produces a new 'HttpHandler' which increments the supplied
-- IORef 'counter' upon each invocation. This is particularly useful for
-- testing.
httpHandlerWithCounter
  :: (MonadIO m)
  => IORef Int -> HttpHandler m -> HttpHandler m
httpHandlerWithCounter counter httpHandler = HttpHandler $ \payload -> do
  liftIO $ modifyIORef' counter succ
  runHttpHandler httpHandler payload

data RequestPayload = RequestPayload
  { boundary :: ByteString
  , commands :: [Some Command]
  }

renderRequestPayload :: RequestPayload -> ByteString
renderRequestPayload RequestPayload{..} =
  body <> "\n--" <> boundary <> "--"
  where

  body = C8.intercalate "\n\n" $ fmap renderCommand commands

  renderCommand :: Some Command -> ByteString
  renderCommand (This (Command k action)) =
    let EncodedRequest key content = encodeRequest k action
    in C8.intercalate "\n"
        [ "--" <> boundary
        , "Content-Type: application/http"
        , "Content-ID: <" <> UUID.toASCIIBytes key <> ">"
        , ""
        , content
        ]

data Command a = Command { key :: UUID, action :: Action a }

data EncodedRequest = EncodedRequest UUID ByteString
  deriving (Eq, Show)

encodeRequest :: UUID -> Action a -> EncodedRequest
encodeRequest key action =
  case mBody of
    Nothing -> EncodedRequest key top
    Just body ->
      EncodedRequest key $
        C8.intercalate "\n"
          [ top
          , "Content-Length: " <> showP (C8.length body)
          , "" -- Extra newline between headers and body.
          , body
          ]
  where
  req = method <> " " <> uri

  method = encodeRequestMethod action

  uri = encodeRequestUri action

  top = C8.intercalate "\n"
    [ req
    , "Authorization: Bearer " <> toBS accessToken
    , "Content-Type: application/json"
    ]

  mBody :: Maybe ByteString
  mBody = L.toStrict <$> case action of
    CalendarsGet {} -> Nothing
    EventsInsert _ _ ev -> Just $ Aeson.encode ev
    EventsInsertWithKey _ _ ev -> Just $ Aeson.encode ev
    EventsUpdate _ _ ev ->  Just $ Aeson.encode ev
    EventsDelete {} -> Nothing
    EventsGet {} -> Nothing

  accessToken :: AccessToken
  accessToken = case action of
    CalendarsGet x _ -> x
    EventsInsert x _ _ -> x
    EventsInsertWithKey x _ _ -> x
    EventsUpdate x _ _ -> x
    EventsDelete x _ _ -> x
    EventsGet x _ _ -> x

encodeRequestMethod :: Action a -> ByteString
encodeRequestMethod = \case
  CalendarsGet        {} -> "GET"
  EventsInsert        {} -> "POST"
  EventsInsertWithKey {} -> "POST"
  EventsUpdate        {} -> "PUT"
  EventsDelete        {} -> "DELETE"
  EventsGet           {} -> "GET"

encodeRequestUri :: Action a -> ByteString
encodeRequestUri a = "/calendar/v3/" <> urlSuffix a
  where
  urlSuffix :: Action a -> ByteString
  urlSuffix = \case
    CalendarsGet _ calId ->
      "calendars/" <> toBS calId

    EventsInsert _ calId _ ->
      "calendars/" <> toBS calId <> "/events"

    EventsInsertWithKey _ calId _ ->
      "calendars/" <> toBS calId <> "/events"

    EventsUpdate _ calId (Keyed evId _) ->
      "calendars/" <> toBS calId <> "/events/" <> toBS evId

    EventsDelete _ calId evId ->
      "calendars/" <> toBS calId <> "/events/" <> toBS evId

    EventsGet _ calId evId ->
      "calendars/" <> toBS calId <> "/events/" <> toBS evId

toBS :: (Coercible a Text) => a -> ByteString
toBS = Text.encodeUtf8 . coerce

-- | Polymorphic show
showP :: (IsString s, Show a) => a -> s
showP = fromString . show

mkBatchedClient
  :: forall m. (MonadIO m)
  => HttpHandler m
  -> Logger
  -> Client m
mkBatchedClient httpHandler logger = Client {runClient}
  where
  runClient :: forall a. Request a -> m a
  runClient req = do
    cmd <- runApComposed genUUID req
    let cmds = FreeAp.runAp_ (\c -> [This c]) cmd
    let reader = FreeAp.runAp mkReader cmd
    env <- runCommands cmds
    case Response.unDecodeResult $ runReaderT reader env of
      Left e -> throwIO e
      Right x -> pure x

  -- Limit imposed by the google batch api.
  batchLimit :: Int
  batchLimit = 50

  -- Create a unique UUID for each request
  genUUID :: Action a -> m (FreeAp.Ap Command a)
  genUUID a = do
    u <- liftIO UUID.nextRandom
    pure $ FreeAp.liftAp $ Command u a

  mkReader :: Command ~> BatchedReader
  mkReader = \case
    Command uuid action -> ReaderT $ \env ->
      case Map.lookup uuid env of
        Just response -> decode action response
        Nothing ->
          Response.decodeFailure $
            "Request " <> show uuid <> " sent but response not received"

  -- We must enumerate all of the GADT constructors here so the compiler knows
  -- which 'Response.Decoder' instance to use.
  decode :: Action a -> EncodedResponse -> Response.DecodeResult a
  decode a r = case a of
    CalendarsGet        {} -> Response.decode r
    EventsInsert        {} -> Response.decode r
    EventsInsertWithKey {} -> Response.decode r
    EventsUpdate        {} -> Response.decode r
    EventsDelete        {} -> Response.decode r
    EventsGet           {} -> Response.decode r

  runCommands :: [Some Command] -> m BatchedEnv
  runCommands cmds = do
    mconcat <$> traverse runChunk (List.chunksOf batchLimit cmds)
    where
    runChunk :: [Some Command] -> m BatchedEnv
    runChunk chunk = do
      boundary <- ("batch_" <>) . UUID.toASCIIBytes <$> liftIO (UUID.nextRandom)
      let payload = RequestPayload boundary chunk
      logPayload payload
      BatchResponse response <- runHttpHandler httpHandler payload
      case Response.parse response of
        Left e -> liftIO $ throwIO e
        Right x -> pure x

  logPayload :: RequestPayload -> m ()
  logPayload RequestPayload{commands} = do
    let formattedActions =
          Text.unlines $
            commands <&> \(This (Command _ action)) ->
              (" * " <>) $ Text.decodeUtf8 $
                encodeRequestMethod action <> " " <> encodeRequestUri action
    withLogger logger $
      $logDebug $
        "Sending " <> showP (length commands) <> " Google requests in a batch:\n"
          <> formattedActions

-- | Type used for keeping track of each command and its associated response.
type BatchedEnv = Map UUID EncodedResponse

-- | Reader Monad for processing
type BatchedReader a = ReaderT BatchedEnv Response.DecodeResult a
