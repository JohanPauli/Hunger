{- |
  Utilities for calculating with dates and times.
-}
module Util.Time
(
-- * Extra Convenience Functions
  parseTime
, parsePosixInt
, utcTimeToPOSIXTimestamp
, posixSecondsToUTCTime

-- Time stuff from the time package
, module T
)
where



-- Time library from Hackage:
import Data.Time.Calendar as T
import Data.Time.Clock as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format as T hiding (parseTime)



-- | Parse a time with the given format string into a UTCTime.
parseTime :: String -- ^ Format string.
          -> String -- ^ Input string.
          -> UTCTime
parseTime = parseTimeOrError True defaultTimeLocale

-- | Parse a time with the given format string as a POSIX timestamp.
parsePosixInt :: String -> String -> Int
parsePosixInt fmt inp = (round . utcTimeToPOSIXSeconds) $ parseTime fmt inp

-- | Convert from UTCTime to a POSIX timestamp.
utcTimeToPOSIXTimestamp :: UTCTime -> Int
utcTimeToPOSIXTimestamp = round . utcTimeToPOSIXSeconds



-- | Make a UTC time object from a POSIX timestamp.
posixSecondsToUTCTime :: Int -> UTCTime
posixSecondsToUTCTime i =
  UTCTime (addDays (toInteger d) unixEpochDay) (realToFrac t)
  where
    (d,t) = divMod i posixDayLength

-- | The unix epoch day, Jan 1, 1970.
unixEpochDay :: Day
unixEpochDay = fromGregorian 1970 1 1

-- | The number of seconds in a day.
posixDayLength :: Int
posixDayLength = 86400
