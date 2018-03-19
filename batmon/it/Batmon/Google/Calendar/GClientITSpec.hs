{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Batmon.Google.Calendar.GClientITSpec where

import Batmon.OffsetTime
import Batmon.Test
import Data.IORef (readIORef)
import qualified Batmon.Google.Calendar as G
import qualified Data.Time.Zones as Time
import qualified Data.Time.Zones.All as Time

spec :: Spec
spec = do
  describe "GClient" $ do
    it "should batch multiple requests" $ do
      (gClient, counter) <- newGClientWithCounter

      readIORef counter `shouldReturn` 0
      tz <- getGoogleTZ gClient
      readAndZeroIORef' counter `shouldReturn` 1

      summary1 <- newEventSummary
      start1 <- truncateToHour . minusDays 1 <$> getCurrentOffsetTime tz
      let end1 = plusHours 1 start1
      let event1 = G.CalendarEvent
            { summary = Just summary1
            , start = G.eventTimeOfDateTime start1
            , end  = G.eventTimeOfDateTime end1
            , status = Nothing
            , attendees = Nothing
            }

      summary2 <- newEventSummary
      let start2 = plusHours 2 start1
      let end2 = plusHours 1 start2
      let event2 = G.CalendarEvent
            { summary = Just summary2
            , start = G.eventTimeOfDateTime start2
            , end = G.eventTimeOfDateTime end2
            , status = Nothing
            , attendees = Nothing
            }

      (res0, res1, res2) <- G.runClient gClient $
        (,,)
          <$> G.mkRequest (G.CalendarsGet gTok calId)
          <*> G.mkRequest (G.EventsInsert gTok calId event1)
          <*> G.mkRequest (G.EventsInsert gTok calId event2)

      readAndZeroIORef' counter `shouldReturn` 1
      e1 <- expectRight res1
      e2 <- expectRight res2

      res0
        `shouldMatchPattern` (\(Right (Just (G.Keyed k _))) -> k)
        `shouldReturn` G.Key envGoogleEmail

      e1 ^. #value `shouldBe` G.CalendarEvent
        { summary = Just summary1
        , start = G.eventTimeOfDateTime start1
        , end = G.eventTimeOfDateTime end1
        , status = Just G.Confirmed
        , attendees = Nothing
        }

      e2 ^. #value `shouldBe` G.CalendarEvent
        { summary = Just summary2
        , start = G.eventTimeOfDateTime start2
        , end = G.eventTimeOfDateTime end2
        , status = Just G.Confirmed
        , attendees = Nothing
        }

      let e1' = e1 & #value . #summary .~ Just "foo"
      let e2' = e2 & #value . #summary .~ Just "bar"

      void $ G.runClient gClient $
           G.mkRequest (G.EventsUpdate gTok calId e1')
        *> G.mkRequest (G.EventsUpdate gTok calId e2')

      readAndZeroIORef' counter `shouldReturn` 1

      (res1upd, res2upd) <- G.runClient gClient $
        (,)
          <$> G.mkRequest (G.EventsGet gTok calId (e1 ^. #key))
          <*> G.mkRequest (G.EventsGet gTok calId (e2 ^. #key))

      readAndZeroIORef' counter `shouldReturn` 1

      e1upd <- expectJust =<< expectRight res1upd
      e2upd <- expectJust =<< expectRight res2upd

      e1upd ^. #value . #summary `shouldBe` Just "foo"
      e2upd ^. #value . #summary `shouldBe` Just "bar"

      expectRight <=< G.runClient gClient $
           G.mkRequest (G.EventsDelete gTok calId (e1 ^. #key))
        *> G.mkRequest (G.EventsDelete gTok calId (e2 ^. #key))

      readAndZeroIORef' counter `shouldReturn` 1

      (res1del, res2del) <- G.runClient gClient $
        (,)
          <$> G.mkRequest (G.EventsGet gTok calId (e1 ^. #key))
          <*> G.mkRequest (G.EventsGet gTok calId (e2 ^. #key))

      readAndZeroIORef' counter `shouldReturn` 1

      e1del <- expectJust =<< expectRight res1del
      e2del <- expectJust =<< expectRight res2del

      e1del ^. #value . #status `shouldBe` Just G.Cancelled
      e2del ^. #value . #status `shouldBe` Just G.Cancelled

    it "EventsInsertWithKey should work" $ do
      (gClient, _) <- newGClientWithCounter

      tz <- getGoogleTZ gClient

      eid <- G.newCalendarEventId
      summary1 <- newEventSummary
      start1 <- truncateToHour . minusDays 1 <$> getCurrentOffsetTime tz
      let event = G.Keyed
            { key = eid
            , value = G.CalendarEvent
                { summary = Just summary1
                , start = G.eventTimeOfDateTime start1
                , end  = G.eventTimeOfDateTime $ plusHours 1 start1
                , status = Just G.Confirmed -- Setting this here to make assertions simpler.
                , attendees = Nothing
                }
            }

      G.runClient gClient
        (G.mkRequest $ G.EventsInsertWithKey gTok calId event)
        `shouldReturn` Right event

      G.runClient gClient (G.mkRequest $ G.EventsGet gTok calId (event ^. #key))
        `shouldReturn` Right (Just event)

      G.runClient gClient (G.mkRequest $ G.EventsDelete gTok calId (event ^. #key))
        `shouldReturn` Right ()

      G.runClient gClient (G.mkRequest $ G.EventsGet gTok calId (event ^. #key))
        `shouldReturn` Right (Just $ event & #value . #status .~ Just G.Cancelled)

    it "EventsGet should return Nothing if not exists" $ do
      eid <- G.newCalendarEventId
      (gClient, _) <- newGClientWithCounter
      G.runClient gClient
        (G.mkRequest $ G.EventsGet gTok calId eid)
        `shouldReturn` Right Nothing

  where
  calId = G.primaryCalendarId
  gTok = envGoogleAccessToken

  getGoogleTZ :: G.Client IO -> IO Time.TZ
  getGoogleTZ gClient = do
    G.runClient gClient (G.mkRequest $ G.CalendarsGet gTok G.primaryCalendarId)
      >>= expectRight
      >>= expectJust
      <&> \cal -> Time.tzByLabel $ cal ^. #value . #timeZone
