module Batmon.Google.Calendar.Business where

import Batmon.Free
import Batmon.JSON
import Batmon.Logger
import Batmon.OffsetTime
import Batmon.Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Some (Some(This))
import Data.Typeable (Typeable)
import qualified Batmon.Google.Calendar as G
import qualified Control.Applicative.Free as FreeAp
import qualified Control.Monad.Free as FreeMonad
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Control.Monad.Trans.Class (lift)

newtype UserId = UserId { unUserId :: UUID }
  deriving newtype (Show, Eq, Ord)

newtype TokenProvider f = TokenProvider
  { runTokenProvider :: [UserId] -> f TokenMap }

type TokenMap = Map UserId G.AccessToken

data BFailure = BFailure
  { message :: Text
  , failureType :: BFailureType
  } deriving (Show, Eq)

toBFailure :: G.ClientFailure -> BFailure
toBFailure G.ClientFailure{status, message} =
  BFailure message $ BFailureStatus status

data BFailureType
  = BFailureStatus Int
  | BFailureMessage
  deriving (Show, Eq)

newtype BEventId = BEventId UUID
  deriving newtype (Show, Eq, ToJSON, FromJSON)

fromBEventId :: BEventId -> G.KeyOf G.CalendarEvent
fromBEventId (BEventId uuid) =
  G.Key $ Text.filter (/= '-') $ UUID.toText uuid

data BEvent = BEvent
  { key :: BEventId
  , title :: Text
  , start :: OffsetTime
  , end :: OffsetTime
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via DefaultJSON BEvent

fromBEvent :: BEvent -> G.Keyed G.CalendarEvent
fromBEvent BEvent{key, title, start, end} =
  G.Keyed
    { key = fromBEventId key
    , value = G.CalendarEvent
        { summary   = Just title
        , start     = G.eventTimeOfDateTime start
        , end       = G.eventTimeOfDateTime end
        , status    = Nothing
        , attendees = Nothing
        }
    }

type BActionResult a = BAction (Either BFailure a)

-- | Business actions
data BAction a where
  BInsertEvent :: UserId -> BEvent   -> BActionResult BInsertResult
  BDeleteEvent :: UserId -> BEventId -> BActionResult BDeleteResult
  BEventExists :: UserId -> BEventId -> BActionResult Bool
  BUpdateEvent :: UserId -> BEvent   -> BActionResult BUpdateResult

deriving stock instance Eq (BAction a)
deriving stock instance Show (BAction a)

getUserIdFromBAction :: BAction a -> UserId
getUserIdFromBAction = \case
  BInsertEvent uid _ -> uid
  BDeleteEvent uid _ -> uid
  BEventExists uid _ -> uid
  BUpdateEvent uid _ -> uid

type BRequest = FreeAp.Ap BAction

data BCommand a = BCommand { key :: BCommandId, action :: BAction a }

mkBRequest :: BAction a -> BRequest a
mkBRequest = FreeAp.liftAp

-- | Client for executing business requests
newtype BClient f = BClient
  { runBClient :: forall a. BRequest a -> f a }

mkGoogleBusinessClient
  :: forall m. (MonadIO m)
  => TokenProvider m
  -> G.Client m
  -> Logger
  -> BClient m
mkGoogleBusinessClient tokenProvider gClient _logger = BClient run
  where
  run :: BRequest a -> m a
  run req = do
    cmd <- runApComposed mkCmd req
    let cmds :: [Some BCommand] = FreeAp.runAp_ (pure . This) cmd
    let userIds = cmds <&> \(This (BCommand _ a)) -> getUserIdFromBAction a
    tokens <- runTokenProvider tokenProvider userIds
    results <- runInterpreter $ mkInputs tokens cmds
    FreeAp.runAp (extractor results) cmd

  runInterpreter :: Inputs -> m IResultMap
  runInterpreter inputs = do
    case allComplete inputs of
      Just results -> pure results
      Nothing -> do
        -- Obtain the next set of steps and their indices so we can run them and
        -- bind the results to the next set of inputs.
        let steps = collectSteps inputs
        response <- G.runClient gClient $ sequenceA steps
        runInterpreter $ response <> inputs

  extractor :: IResultMap -> BCommand a -> m a
  extractor results (BCommand k a) = case a of
    action@(BInsertEvent {}) -> case res of
      This (IResult (BInsertEvent {}) x) -> pure x
      This (IResult other _) -> typeMismatch action other

    action@(BDeleteEvent {}) -> case res of
      This (IResult (BDeleteEvent {}) x) -> pure x
      This (IResult other _) -> typeMismatch action other

    action@(BEventExists {}) -> case res of
      This (IResult (BEventExists {}) x) -> pure x
      This (IResult other _) -> typeMismatch action other

    action@(BUpdateEvent {}) -> case res of
      This (IResult (BUpdateEvent {}) x) -> pure x
      This (IResult other _) -> typeMismatch action other

    where
    res = case Map.lookup k results of
      Nothing -> throw $ userError "Unexpected missing action result!"
      Just x -> x

    typeMismatch :: BAction a -> BAction b -> c
    typeMismatch expected actual =
      throw $ userError $
        "Type mismatch! Expected " <> show expected <> "; got: " <> show actual

  mkCmd :: BAction a -> m (FreeAp.Ap BCommand a)
  mkCmd a = do
    u <- liftIO UUID.nextRandom
    pure $ FreeAp.liftAp $ BCommand (BCommandId u) a

  mkInputs :: TokenMap -> [Some BCommand] -> Inputs
  mkInputs tokens cmds = Map.fromList $ cmds <&> \(This (BCommand cmdId a)) ->
    (cmdId, This <$> mkGLogic tokens a)

  mkGLogic :: TokenMap -> BAction a -> GLogic (IResult a)
  mkGLogic tokens = \case
    action@(BInsertEvent uid event) -> go action insertEvent uid event
    action@(BDeleteEvent uid eid)   -> go action deleteEvent uid eid
    action@(BEventExists uid eid)   -> go action eventExists uid eid
    action@(BUpdateEvent uid event) -> go action updateEvent uid event
    where
    tok uid =
      fromMaybe
        (throw $ userError $ "Google token not found for user " <> show uid)
        (Map.lookup uid tokens)

    go :: BActionResult b
       -> (G.AccessToken -> c -> GAttempt b)
       -> UserId
       -> c -- ^ Polymorphic to support BEvent or BEventId
       -> GLogic (IResult (Either BFailure b))
    go action f uid c = fmap (IResult action) . runExceptT $ f (tok uid) c

  calId = G.primaryCalendarId

  insertEvent :: G.AccessToken -> BEvent -> GAttempt BInsertResult
  insertEvent tok bEvent = do
    let gEvent = fromBEvent bEvent
    gAttempt (G.EventsInsertWithKey tok calId gEvent) >>= \case
      Right (G.Keyed k _) -> pure $ Inserted $ coerce k
      Left e0 -> do
        when ((e0 ^. #status) /= 409) $ throwError $ toBFailure e0
        gAttempt (G.EventsUpdate tok calId gEvent) >>= \case
          Right (G.Keyed k _) -> pure $ UpdatedViaInsert $ coerce k
          Left e1 -> throwError $ toBFailure e1

  deleteEvent :: G.AccessToken -> BEventId -> GAttempt BDeleteResult
  deleteEvent tok eid = do
    let gId = fromBEventId eid
    gAttempt (G.EventsDelete tok calId gId) >>= \case
      Right _ -> pure $ Deleted $ coerce gId
      Left e -> do
        case e ^. #status of
          404 -> pure $ NotFound $ coerce gId
          410 -> pure $ AlreadyDeleted $ coerce gId
          _   -> throwError $ toBFailure e

  eventExists :: G.AccessToken -> BEventId -> GAttempt Bool
  eventExists tok eid = do
    gAttempt (G.EventsGet tok calId (fromBEventId eid)) >>=
      either (throwError . toBFailure) (\case
        Nothing -> pure False
        Just gEvent -> pure $ gEvent ^. #value . #status /= Just G.Cancelled)

  updateEvent :: G.AccessToken -> BEvent -> GAttempt BUpdateResult
  updateEvent tok bEvent = do
    let gEvent = fromBEvent bEvent
    gAttempt (G.EventsUpdate tok calId gEvent) >>= \case
      Right (G.Keyed k _) -> pure $ Updated $ coerce k
      Left e0 -> do
        when ((e0 ^. #status) /= 404) $ throwError $ toBFailure e0
        gAttempt (G.EventsInsertWithKey tok calId gEvent) >>= \case
          Right (G.Keyed k _) -> pure $ InsertedViaUpdate $ coerce k
          Left e1 -> throwError $ toBFailure e1

  gAttempt :: G.Action a -> GAttempt a
  gAttempt = lift . FreeMonad.liftF . G.mkRequest

  -- Returns Nothing if there are any Incomplete values; otherwise returns
  -- the final IResultMap.
  allComplete :: Inputs -> Maybe IResultMap
  allComplete = traverse $ \case
    FreeMonad.Pure v -> Just v
    FreeMonad.Free _ -> Nothing

  collectSteps :: Inputs -> Map BCommandId (GStep (GLogic (Some IResult)))
  collectSteps = Map.mapMaybe $ \case
    FreeMonad.Free x -> Just x
    FreeMonad.Pure _ -> Nothing

newtype BCommandId = BCommandId UUID
  deriving newtype (Show, Eq, Ord)

-- | Inputs into the interpreter
type Inputs = Map BCommandId (GLogic (Some IResult))

data IResult a = IResult { action :: BAction a, value :: a }

-- | Final result mapping from the interpreter
type IResultMap = Map BCommandId (Some IResult)

-- | Alias for a Google Request; represents a single "step" in the interpreter.
type GStep = G.Request

-- | Represents a monadic "program" containing several 'GStep's.
-- The interpreter will batch intermediate 'GStep's of multiple 'GLogic'
-- programs to optimize API usage.
type GLogic = FreeMonad.Free GStep

-- | GLogic lifted to deal with failures.
type GAttempt = ExceptT BFailure GLogic

newtype ExtId = ExtId Text
  deriving newtype (Eq, Show)

-- | Result of a BInsertEvent request.
data BInsertResult
  = Inserted ExtId
  | UpdatedViaInsert ExtId
  deriving stock (Eq, Show, Typeable)

-- | Result of a BUpdateEvent request.
data BUpdateResult
  = Updated ExtId
  | InsertedViaUpdate ExtId
  deriving stock (Eq, Show, Typeable)

-- | Result of a BDeleteEvent request.
data BDeleteResult
  = Deleted ExtId
  | AlreadyDeleted ExtId
  | NotFound ExtId
  deriving stock (Eq, Show, Typeable)
