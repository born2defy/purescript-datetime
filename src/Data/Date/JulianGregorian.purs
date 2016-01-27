module Data.Date.JulianGregorian where

import Prelude
import Data.Date.NativeDate
import Data.Time
import Data.Date.Locale
import Data.Tuple (Tuple(..), fst, snd, curry)
import Data.Int (floor, toNumber)
import Data.Ord (min)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List(), fromFoldable, uncons)
import Control.Monad.Eff (Eff())
import Data.Enum (fromEnum)
import Data.Function (on)
import Data.Foldable (fold)
import Data.Date.Utilities.HasDecimal (decPart, getInt, getDecPart)

calcTaxEscrows :: GregorianDate -> Int
calcTaxEscrows d = case d of
  _ | pmtMonth <  November -> 14 - (November `subMonth` pmtMonth)
    | pmtMonth == November -> 14
    | otherwise            -> 2
  where
  subMonth = sub `on` fromEnum
  pmtMonth = asMonth $ (monthAsInt d.month) + 1

-- | Converts an Month into its traditional numeric form
monthAsInt :: Month -> Int
monthAsInt = (+ 1) <<< fromEnum

-- | Julian Day.
-- | The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
-- | The plan is to start with Gregorian since it is easy to enter, then convert to Julian for any date calculations.
newtype JulianTime = JulianTime Number

runJulianTime :: JulianTime -> Number
runJulianTime (JulianTime n) = n

instance showJulianTime :: Show JulianTime where
  show (JulianTime n) = "Julian Time " <> show n

instance eqJulianTime:: Eq JulianTime where
  eq (JulianTime x) (JulianTime y) = x == y

instance ordJulianTime :: Ord JulianTime where
  compare (JulianTime x) (JulianTime y) = compare x y

-- getTimeOfDay :: JulianTime ->

addDaysJT :: Int -> JulianTime -> JulianTime
addDaysJT x (JulianTime y) = JulianTime (toNumber x + y)

diffInDays :: JulianTime -> JulianTime -> Int
diffInDays (JulianTime x) (JulianTime y) = floor (x - y)

diffInDaysGregorian :: GregorianDate -> GregorianDate -> Int
diffInDaysGregorian x y =  (diffInDays `on` gregorianToJulianTime) x y

-- | convert to ISO 8601 Ordinal Date format. First element of result is year (proleptic Gregoran calendar),
-- second is the day of the year, with 1 for Jan 1, and 365 (or 366 in leap years) for Dec 31.
julianTimeToOrdinal :: JulianTime -> Tuple Int Int
julianTimeToOrdinal (JulianTime time) = Tuple year yd
  where
  jday = floor time
  a = jday + 678575
  quadcent = div a 146097
  b = mod a 146097
  cent = min (div b 36524) 3
  c = b - (cent * 36524)
  quad = div c 1461
  d = mod c 1461
  y = min (div d 365) 3
  yd = d - (y * 365) + 1
  year = quadcent * 400 + cent * 100 + quad * 4 + y + 1

-- | convert from ISO 8601 Ordinal Date format.
-- Invalid day numbers will be clipped to the correct range (1 to 365 or 366).
ordinalToJulianTime :: Tuple Int Int -> JulianTime
ordinalToJulianTime (Tuple year day) = JulianTime time
  where
  y = year - 1
  jday = (clip 1 (if isLeapYear (asYear year) then 366 else 365) day) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678576
  time = toNumber jday

julianTimeToGregorian :: JulianTime -> GregorianDate
julianTimeToGregorian time =
  case julianTimeToOrdinal time of
    Tuple y d ->
      let year = Year y
          leap = isLeapYear year
          mAndD = dayOfYearToMonthAndDay leap d
          month = asMonth $ fst mAndD
          day   = asDayOfMonth (snd mAndD) month year
      in  { month, day, year }

-- | convert from proleptic Gregorian calendar. First argument is year, second month number (1-12), third day (1-31).
-- Invalid values will be clipped to the correct range, month first, then day.
gregorianToJulianTime :: GregorianDate -> JulianTime
gregorianToJulianTime gdate = curry ordinalToJulianTime (runYear gdate.year) (monthAndDayToDayOfYear (isLeapYear gdate.year) ((fromEnum gdate.month) + 1) (runDayOfMonth gdate.day))

diffGDates :: GregorianDate -> GregorianDate -> Int
diffGDates x y =  (diffInDays `on` gregorianToJulianTime) x y

showJulianTimeAsDateAndTime :: TimeZoneOffset -> JulianTime -> String
showJulianTimeAsDateAndTime offset j = showDate j <> " " <> showTime j
  where
  showDate = prettyJulianTime
  showTime =
    showTimeOfDayStd
    <<< timeToStdFmt
    <<< toTimeOfDay offset
    <<< asDays
    <<< getDecPart
    <<< runJulianTime

-- | A type representing the time of day
type TimeOfDay
  = { hours :: HourOfDay
    , minutes :: MinuteOfHour
    , seconds :: SecondOfMinute
    , milliseconds :: MillisecondOfSecond
    }

showTimeOfDay :: TimeOfDay -> String
showTimeOfDay s =
  "{ hours: " <> show s.hours
  <> ", minutes " <> show s.minutes
  <> ", seconds" <> show s.seconds
  <> ", milliseconds" <> show s.milliseconds
  <> " }"

type TimeZoneOffset = Int

-- | Takes a partial day and converts it into a time of day
toTimeOfDay :: TimeZoneOffset -> Days -> TimeOfDay
toTimeOfDay offset d = { hours, minutes, seconds, milliseconds }
  where
  totHours     = toHours d + (asHours $ toNumber offset)
  hours        = asHourOfDay           <<< getInt  $ totHours
  totMins      = toMinutes             <<< decPart $ totHours
  minutes      = asMinuteOfHour        <<< getInt  $ totMins
  totSecs      = toSeconds             <<< decPart $ totMins
  seconds      = asSecondOfMinute      <<< getInt  $ totSecs
  totMsecs     = toMilliseconds        <<< decPart $ totSecs
  milliseconds = asMillisecondOfSecond <<< getInt  $ totMsecs


-- | AM or PM
data AMPM = AM | PM

instance showAMPM :: Show AMPM where
  show AM = "AM"
  show PM = "PM"

-- | A type for displaying the time of day
type TimeOfDayString
  = { hoursStr        :: String
    , minutesStr      :: String
    , secondsStr      :: String
    , millisecondsStr :: String
    }

-- A refined version of TimeOfDay which puts the time into std AM/PM format
type TimeStdFmt
  = { time :: TimeOfDayString
    , ampm :: String
    }

timeToStdFmt :: TimeOfDay -> TimeStdFmt
timeToStdFmt t =
  let ampm = show $ if hrs < 12 then AM else PM
      hours = if hrs > 12 then hrs - 12 else if hrs == 0 then 12 else hrs
      hrs   = runHourOfDay t.hours
      time = { hoursStr: show hours
             , minutesStr: show $ runMinuteOfHour t.minutes
             , secondsStr: show $ runSecondOfMinute t.seconds
             , millisecondsStr: show $ runMillisecondOfSecond t.milliseconds
             }
  in  { time, ampm }

showTimeOfDayStd :: TimeStdFmt -> String
showTimeOfDayStd t = fold [ hrs,":",mins," ",ampm," ",secs,":",msecs ]
  where
  hrs = t.time.hoursStr
  mins = t.time.minutesStr
  ampm = t.ampm
  secs = t.time.secondsStr
  msecs = t.time.millisecondsStr


type GregorianDate
  = { month :: Month
    , day   :: DayOfMonth
    , year  :: Year
    }

showGregorianDate :: GregorianDate -> String
showGregorianDate d =
  show d.month <> " "
  <> show (runDayOfMonth d.day) <> ", "
  <> show (runYear d.year)

mkGregorianDate :: Int -> Int -> Int -> GregorianDate
mkGregorianDate m d y = { month, day, year }
  where
  month = asMonth m
  year  = asYear y
  day   = asDayOfMonth d month year

-- | convert month and day in the Gregorian or Julian calendars to day of year.
-- First arg is leap year flag
monthAndDayToDayOfYear :: Boolean -> Int -> Int -> Int
monthAndDayToDayOfYear isLeap month day = (div (367 * month' - 362) 12) + k + day'
  where
  month' = clip 1 12 month
  day' = clip 1 (daysInMonth isLeap (asMonth month')) day
  k = if month' <= 2 then 0 else if isLeap then -1 else -2

-- | convert day of year in the Gregorian or Julian calendars to month and day.
-- First arg is leap year flag
dayOfYearToMonthAndDay :: Boolean -> Int -> Tuple Int Int
dayOfYearToMonthAndDay isLeap yd = findMonthDay (monthLengths isLeap) (clip 1 (if isLeap then 366 else 365) yd)

findMonthDay :: List Int -> Int -> Tuple Int Int
findMonthDay months yd =
  case uncons months of
    Just xs | yd > xs.head -> (\(Tuple m d) -> Tuple (m + 1) d) (findMonthDay xs.tail (yd - xs.head))
    _ -> Tuple 1 yd

monthLengths :: Boolean -> List Int
monthLengths isleap = fromFoldable $
    [31,if isleap then 29 else 28,31,30,31,30,31,31,30,31,30,31]
    --J        F                   M  A  M  J  J  A  S  O  N  D

clip :: forall t. (Ord t) => t -> t -> t -> t
clip a _ x | x < a = a
clip _ b x | x > b = b
clip _ _ x = x


gregorianToDate :: forall e. GregorianDate -> Eff (now :: Now, locale :: Locale | e) Date
gregorianToDate d = do
  currentDate <- now
  maybeDate   <- date d.year d.month d.day
  return $ fromMaybe currentDate maybeDate

dateToGregorian :: Date -> GregorianDate
dateToGregorian = julianTimeToGregorian <<< dateToJulianTime

dateToJulianTime :: Date -> JulianTime
dateToJulianTime = JulianTime <<< (+ 40587.0) <<< runDays <<< toDays <<< toEpochMilliseconds

julianTimeToDate :: JulianTime -> Maybe Date
julianTimeToDate = fromEpochMilliseconds <<< toMilliseconds <<< asDays <<< (`sub` 40587.0) <<< runJulianTime

prettyJulianTime :: JulianTime -> String
prettyJulianTime = showGregorianDate <<<julianTimeToGregorian

msToDayInt :: Number -> Int
msToDayInt = floor <<< (/ 24.0) <<< (/ 3600000.0)

dayIntToMS :: Int -> Number
dayIntToMS = toNumber <<< (* 3600000) <<< (* 24)

runMS :: Milliseconds -> Number
runMS (Milliseconds x) = x
