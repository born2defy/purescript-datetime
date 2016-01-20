module Data.Date.JulianGregorian (
    JulianDay(..), julianToOrdinal, addJDays, diffJDays, dayOfYearToMonthAndDay, dateToJulian, runJulianDay, ordinalToJulian
  , GregorianDate(), julianToGregorian, showGregorianDate, dayOfYearToMonthAndDay, findMonthDay
  , monthLengths, gregorianToDate, mkGregorianDate, gregorianToJulian, julianToDate, monthAndDayToDayOfYear, runMS
  , clip, prettyJulian, dateToGregorian, diffGDates
  , escrows
  ) where

import Prelude
import Data.Date
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

escrows :: GregorianDate -> Int
escrows d = case d of
  _ | pmtMonth < November -> 14 - (November `subMonth` pmtMonth)
    | pmtMonth == November -> 14
    | otherwise -> 2
  where
  subMonth = sub `on` fromEnum
  pmtMonth = asMonth $ (fromEnum month) + 3
  month = d.month

-- | Julian Day.
-- | The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
-- | The plan is to start with Gregorian since it is easy to enter, then convert to Julian for any date calculations.
newtype JulianDay = JulianDay Int

runJulianDay :: JulianDay -> Int
runJulianDay (JulianDay d) = d

instance showJulianDay :: Show JulianDay where
  show (JulianDay d) = "Julian Day " <> show d

instance eqJulianDay:: Eq JulianDay where
  eq (JulianDay x) (JulianDay y) = x == y

instance ordJulianDay :: Ord JulianDay where
  compare (JulianDay x) (JulianDay y) = compare x y

addJDays :: Int -> JulianDay -> JulianDay
addJDays x (JulianDay y) = JulianDay (x + y)

diffJDays :: JulianDay -> JulianDay -> Int
diffJDays (JulianDay x) (JulianDay y) = x - y

diffGDates :: GregorianDate -> GregorianDate -> Int
diffGDates x y =  (diffJDays `on` gregorianToJulian) x y

-- | convert to ISO 8601 Ordinal Date format. First element of result is year (proleptic Gregoran calendar),
-- second is the day of the year, with 1 for Jan 1, and 365 (or 366 in leap years) for Dec 31.
julianToOrdinal :: JulianDay -> Tuple Int Int
julianToOrdinal (JulianDay jday) = Tuple year yd
  where
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
ordinalToJulian :: Tuple Int Int -> JulianDay
ordinalToJulian (Tuple year day) = JulianDay jday
  where
  y = year - 1
  jday = (clip 1 (if isLeapYear (asYear year) then 366 else 365) day) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678576


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

julianToGregorian :: JulianDay -> GregorianDate
julianToGregorian jday =
  case julianToOrdinal jday of
    Tuple y d ->
      let year = Year y
          leap = isLeapYear year
          mAndD = dayOfYearToMonthAndDay leap d
          month = asMonth $ fst mAndD
          day   = asDayOfMonth (snd mAndD) month year
      in  { month, day, year }

mkGregorianDate :: Int -> Int -> Int -> GregorianDate
mkGregorianDate m d y = { month, day, year }
  where
  month = asMonth m
  year  = asYear y
  day   = asDayOfMonth d month year

-- | convert from proleptic Gregorian calendar. First argument is year, second month number (1-12), third day (1-31).
-- Invalid values will be clipped to the correct range, month first, then day.
gregorianToJulian :: GregorianDate -> JulianDay
gregorianToJulian gdate = curry ordinalToJulian (runYear gdate.year) (monthAndDayToDayOfYear (isLeapYear gdate.year) ((fromEnum gdate.month) + 1) (runDayOfMonth gdate.day))

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
dateToGregorian = julianToGregorian <<< dateToJulian


dateToJulian :: Date -> JulianDay
dateToJulian = JulianDay <<< (+ 40587) <<< msToDay <<< runMS <<< toEpochMilliseconds

julianToDate :: JulianDay -> Maybe Date
julianToDate = fromEpochMilliseconds <<< Milliseconds <<< dayToMS <<< (`sub` 40587) <<< runJulianDay

prettyJulian :: JulianDay -> String
prettyJulian = showGregorianDate <<<julianToGregorian

msToDay :: Number -> Int
msToDay = floor <<< (/ 24.0) <<< (/ 3600000.0)

dayToMS :: Int -> Number
dayToMS = toNumber <<< (* 3600000) <<< (* 24)

runMS :: Milliseconds -> Number
runMS (Milliseconds x) = x
