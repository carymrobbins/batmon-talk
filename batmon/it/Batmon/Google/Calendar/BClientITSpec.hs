module Batmon.Google.Calendar.BClientITSpec where

import Batmon.Google.Calendar.Business
import Batmon.OffsetTime
import Batmon.Test
import Data.Coerce (coerce)
import Data.Dynamic (Dynamic)
import Data.Foldable (traverse_)
import Data.Some (Some(This), withSome)
import Data.Typeable (Typeable)
import System.Random (randomRIO)
import qualified Batmon.Google.Calendar as G
import qualified Data.Dynamic as Dynamic
import qualified Data.Map.Strict as Map
import qualified Data.Time.Zones as Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

spec :: Spec
spec = do
  describe "BClient" $ do
    it "should batch efficiently" $ do
      (gClient, counter) <- newGClientWithCounter
      e1 <- newBEvent
      e2 <- newBEvent
      e3 <- newBEvent
      let bClient = defaultBClient gClient

      runBClient bClient (
        (,,)
          <$> mkBRequest (BEventExists userId (e1 ^. #key))
          <*> mkBRequest (BEventExists userId (e2 ^. #key))
          <*> mkBRequest (BEventExists userId (e3 ^. #key)))
        `shouldReturn`
          ( Right False
          , Right False
          , Right False
          )

      readAndZeroIORef' counter `shouldReturn` 1

      runBClient bClient (
        (,,)
          <$> mkBRequest (BInsertEvent userId e1)
          <*> mkBRequest (BUpdateEvent userId e2)
          <*> mkBRequest (BUpdateEvent userId e3))
        `shouldReturn`
          ( Right $ Inserted          $ getExtId e1
          , Right $ InsertedViaUpdate $ getExtId e2
          , Right $ InsertedViaUpdate $ getExtId e3
          )

      readAndZeroIORef' counter `shouldReturn` 2

      runBClient bClient (
        (,,)
          <$> mkBRequest (BDeleteEvent userId (e1 ^. #key))
          <*> mkBRequest (BDeleteEvent userId (e2 ^. #key))
          <*> mkBRequest (BDeleteEvent userId (e3 ^. #key)))
        `shouldReturn`
          ( Right $ Deleted $ getExtId e1
          , Right $ Deleted $ getExtId e2
          , Right $ Deleted $ getExtId e3
          )

      readAndZeroIORef' counter `shouldReturn` 1

    it "should work with large inputs" $ do
      (gClient, counter) <- newGClientWithCounter
      let bClient = defaultBClient gClient

      actions <- replicateM 100 newBAction

      let req = sequenceA $ dynBRequest <$> actions

      res <- runBClient bClient req

      length res `shouldBe` 100

      n <- readAndZeroIORef' counter

      debug $ "Large input batch count: " <> show n

      when (n > 4) $ do
        expectationFailure $ "Batch count exceeded 4, was " <> show n

      traverse_ validateDynResponse $ zip actions res

  where
  userId = UserId UUID.nil

  defaultBClient :: G.Client IO -> BClient IO
  defaultBClient gClient =
    mkGoogleBusinessClient defaultTokenProvider gClient logger

  defaultTokenProvider :: TokenProvider IO
  defaultTokenProvider =
    TokenProvider $ const $ pure $
      Map.singleton userId envGoogleAccessToken

  newBEventId :: IO BEventId
  newBEventId = BEventId <$> UUID.nextRandom

  newBEvent :: IO BEvent
  newBEvent = do
    key <- newBEventId
    title <- newEventSummary
    start <- truncateToHour . minusDays 1 <$> getCurrentOffsetTime Time.utcTZ
    pure BEvent { key, title, start, end = plusHours 1 start }

  toExtId :: BEventId -> ExtId
  toExtId = coerce . fromBEventId

  getExtId :: BEvent -> ExtId
  getExtId bEvent = toExtId $ bEvent ^. #key

  newBAction :: IO (Some BAction)
  newBAction = randomRIO @Int (0, 3) >>= \case
    0 -> This . BInsertEvent userId <$> newBEvent
    1 -> This . BDeleteEvent userId <$> newBEventId
    2 -> This . BEventExists userId <$> newBEventId
    3 -> This . BUpdateEvent userId <$> newBEvent
    n -> throw $ userError $ "impossible: " <> show n

  dynBRequest :: Some BAction -> BRequest Dynamic
  dynBRequest s = withSome s $ \case
    x@(BInsertEvent _ _) -> Dynamic.toDyn <$> mkBRequest x
    x@(BDeleteEvent _ _) -> Dynamic.toDyn <$> mkBRequest x
    x@(BEventExists _ _) -> Dynamic.toDyn <$> mkBRequest x
    x@(BUpdateEvent _ _) -> Dynamic.toDyn <$> mkBRequest x

  validateDynResponse :: (Some BAction, Dynamic) -> IO ()
  validateDynResponse (sa, d) = withSome sa $ \case
    x@(BInsertEvent _ ev) ->
      case proxyDynResponse x d of
        Nothing -> expectationFailure $ "Failed to decode " <> show x
        Just (Left e) -> do
          debug $ "Ignoring a client failure for " <> show x <> ": " <> show e
        Just (Right res@(Inserted extId)) -> do
          debug $ show x <> ": " <>  show res
          extId `shouldBe` (getExtId ev)
        Just (Right res@(UpdatedViaInsert extId)) -> do
          debug $ show x <> ": " <>  show res
          extId `shouldBe` (getExtId ev)

    x@(BEventExists _ _) ->
      case proxyDynResponse x d of
        Nothing -> expectationFailure $ "Failed to decode " <> show x
        Just (Left e) -> do
          debug $ "Ignoring a client failure for " <> show x <> ": " <> show e
        Just (Right res) -> do
          debug $ show x <> ": " <>  show res

    x@(BDeleteEvent _ eid) ->
      case proxyDynResponse x d of
        Nothing -> expectationFailure $ "Failed to decode " <> show x
        Just (Left e) -> do
          debug $ "Ignoring a client failure for " <> show x <> ": " <> show e
        Just (Right res@(Deleted extId)) -> do
          debug $ show x <> ": " <>  show res
          extId `shouldBe` (toExtId eid)
        Just (Right res@(AlreadyDeleted extId)) -> do
          debug $ show x <> ": " <>  show res
          extId `shouldBe` (toExtId eid)
        Just (Right res@(NotFound extId)) -> do
          debug $ show x <> ": " <>  show res
          extId `shouldBe` (toExtId eid)

    x@(BUpdateEvent _ ev) ->
      case proxyDynResponse x d of
        Nothing -> expectationFailure $ "Failed to decode " <> show x
        Just (Left e) -> do
          debug $ "Ignoring a client failure for " <> show x <> ": " <> show e
        Just (Right res@(Updated extId)) -> do
          debug $ show x <> ": " <>  show res
          extId `shouldBe` (getExtId ev)
        Just (Right res@(InsertedViaUpdate extId)) -> do
          debug $ show x <> ": " <>  show res
          extId `shouldBe` (getExtId ev)

  proxyDynResponse :: (Typeable a) => BAction a -> Dynamic -> Maybe a
  proxyDynResponse _ = Dynamic.fromDynamic
