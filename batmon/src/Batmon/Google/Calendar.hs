module Batmon.Google.Calendar where

import Batmon.JSON
import Batmon.OffsetTime
import Batmon.Prelude
import Batmon.TextConversion
import Control.Exception (throw)
import Data.Aeson (ToJSON, FromJSON, (.:))
import Data.Kind (Constraint)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Zones.All (TZLabel)
import qualified Control.Applicative.Free as Free
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

newtype AccessToken = AccessToken { unAccessToken :: Text }
  deriving newtype (Show, Eq)

newtype Key a k = Key { unKey :: k }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

type family KeyOf a

data Keyed a = Keyed
  { key :: KeyOf a
  , value :: a
  } deriving stock (Generic)

type Derive'Keyed f a = ((f (KeyOf a), f a) :: Constraint)

deriving stock instance (Derive'Keyed Eq a) => Eq (Keyed a)
deriving stock instance (Derive'Keyed Show a) => Show (Keyed a)

instance (Derive'Keyed ToJSON a) => ToJSON (Keyed a) where
  toJSON (Keyed k v) = case Aeson.toJSON v of
    Aeson.Object o -> Aeson.Object $ HashMap.insert "id" (Aeson.toJSON k) o
    x -> throw $ userError $ "Expectd Keyed to serialize to object, got: " <> show x

instance (Derive'Keyed FromJSON a) => FromJSON (Keyed a) where
  parseJSON v = flip (Aeson.withObject "Keyed") v $ \o ->
    Keyed <$> (o .: "id") <*> Aeson.parseJSON v

data Calendar = Calendar
  { timeZone :: TZLabel }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DefaultJSON Calendar

type instance KeyOf Calendar = Key Calendar Text

primaryCalendarId :: Key Calendar Text
primaryCalendarId = Key "primary"

data CalendarEvent = CalendarEvent
  { summary :: Maybe Text
  , start :: EventTime
  , end :: EventTime
  , status :: Maybe CalendarEventStatus
  , attendees :: Maybe (Set Attendee)
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via DefaultJSON CalendarEvent

type instance KeyOf CalendarEvent = Key CalendarEvent Text

-- | Generate a new CalendarEvent id using UUID V4 generation.
newCalendarEventId :: IO (KeyOf CalendarEvent)
newCalendarEventId =
  Key . Text.replace "-" "" . UUID.toText <$> UUID.nextRandom

data EventTime = EventTime
  { date :: Maybe Time.Day
  , dateTime :: Maybe OffsetTime
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via DefaultJSON EventTime

eventTimeOfDate :: Time.Day -> EventTime
eventTimeOfDate d = EventTime (Just d) Nothing

eventTimeOfDateTime :: OffsetTime -> EventTime
eventTimeOfDateTime zt = EventTime Nothing (Just zt)

data CalendarEventStatus = Confirmed | Tentative | Cancelled
  deriving stock (Show, Eq, Generic, Bounded, Enum)
  deriving (TextConversion) via DefaultTextConversion CalendarEventStatus
  deriving (ToJSON, FromJSON) via TextConversionJSON CalendarEventStatus

newtype Attendee = Attendee { email :: Email }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via DefaultJSON Attendee

newtype Email = Email { unEmail :: Text }
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data ClientFailure = ClientFailure
  { status :: Int
  , message :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via DefaultJSON ClientFailure

type ActionResult a = Action (Either ClientFailure a)

data Action a where
  CalendarsGet
    :: AccessToken
    -> KeyOf Calendar
    -> ActionResult (Maybe (Keyed Calendar))

  EventsInsert
    :: AccessToken
    -> KeyOf Calendar
    -> CalendarEvent
    -> ActionResult (Keyed CalendarEvent)

  EventsInsertWithKey
    :: AccessToken
    -> KeyOf Calendar
    -> Keyed CalendarEvent
    -> ActionResult (Keyed CalendarEvent)

  EventsUpdate
    :: AccessToken
    -> KeyOf Calendar
    -> Keyed CalendarEvent
    -> ActionResult (Keyed CalendarEvent)

  EventsDelete
    :: AccessToken
    -> KeyOf Calendar
    -> KeyOf CalendarEvent
    -> ActionResult ()

  EventsGet
    :: AccessToken
    -> KeyOf Calendar
    -> KeyOf CalendarEvent
    -> ActionResult (Maybe (Keyed CalendarEvent))

type Request = Free.Ap Action

mkRequest :: Action a -> Request a
mkRequest = Free.liftAp

newtype Client f = Client { runClient :: forall a. Request a -> f a }
