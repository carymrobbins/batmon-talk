module Batmon.OffsetTime
  ( OffsetTime(offsetTimeToZoned)
  , getCurrentOffsetTime
  , offsetTimeToUTCTime
  , offsetTimeFromUTCTime
  , offsetTimeFromZoned
  , offsetTimeAlaUTC
  , truncateToHour
  , plusDays
  , minusDays
  , plusHours
  , minusHours
  ) where

import Batmon.Orphans ()
import Control.Lens ((.~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Function ((&))
import Data.Generics.Labels ()
import Prelude
import qualified Data.Time as Time
import qualified Data.Time.Zones as Time

-- | Wrapper around 'ZonedTime' which only preserves the offset of the zone.
-- This better reflects how Google treats our zoned times.
newtype OffsetTime = UnsafeMkOffsetTime { offsetTimeToZoned :: Time.ZonedTime }
  deriving newtype (Show, ToJSON, FromJSON)

-- | Compares only times and offsets.
instance Eq OffsetTime where
  (==)
    (UnsafeMkOffsetTime (Time.ZonedTime localTime1 (Time.TimeZone tzMinutes1 _ _)))
    (UnsafeMkOffsetTime (Time.ZonedTime localTime2 (Time.TimeZone tzMinutes2 _ _)))
      = (localTime1, tzMinutes1) == (localTime2, tzMinutes2)

getCurrentOffsetTime :: Time.TZ -> IO OffsetTime
getCurrentOffsetTime z = offsetTimeFromUTCTime z <$> Time.getCurrentTime

offsetTimeToUTCTime :: OffsetTime -> Time.UTCTime
offsetTimeToUTCTime = Time.zonedTimeToUTC . offsetTimeToZoned

offsetTimeFromUTCTime :: Time.TZ -> Time.UTCTime -> OffsetTime
offsetTimeFromUTCTime z t =
  offsetTimeFromZoned $ Time.utcToZonedTime (Time.timeZoneForUTCTime z t) t

-- | Strips zone info, only preserving the offset.
offsetTimeFromZoned :: Time.ZonedTime -> OffsetTime
offsetTimeFromZoned zt = UnsafeMkOffsetTime $
  zt & #zonedTimeZone . #timeZoneName .~ ""
     & #zonedTimeZone . #timeZoneSummerOnly .~ False

-- | Modify an 'OffsetTime' as if it were a UTCTime.
offsetTimeAlaUTC
  :: (Time.UTCTime -> Time.UTCTime)
  -> OffsetTime
  -> OffsetTime
offsetTimeAlaUTC f (UnsafeMkOffsetTime zt) =
  UnsafeMkOffsetTime $
    Time.utcToZonedTime (Time.zonedTimeZone zt) $
      f (Time.zonedTimeToUTC zt)

truncateToHour :: OffsetTime -> OffsetTime
truncateToHour = offsetTimeAlaUTC $ \t ->
  let hours = floor (toRational (Time.utctDayTime t)) `div` 3600
  in t { Time.utctDayTime = fromInteger $ hours * 3600 }

plusDays :: Int -> OffsetTime -> OffsetTime
plusDays n =
  offsetTimeAlaUTC $ Time.addUTCTime $ Time.nominalDay * fromIntegral n

minusDays :: Int -> OffsetTime -> OffsetTime
minusDays n = plusDays (-n)

plusHours :: Int -> OffsetTime -> OffsetTime
plusHours n =
  offsetTimeAlaUTC $ Time.addUTCTime $ fromIntegral (n * 3600)

minusHours :: Int -> OffsetTime -> OffsetTime
minusHours n = plusHours (-n)
