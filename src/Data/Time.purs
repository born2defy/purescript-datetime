module Data.Time where

import Prelude
import Data.Enum (Enum, Cardinality(..), fromEnum, defaultSucc, defaultPred, toEnum)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time
import Data.Int.Extensions (absInt)
import Data.Date.Utilities.Clamped (Clamped, clamp)

------------------Years--------------------------
-- | A year date component value.
newtype Year = Year Int

instance showYear :: Show Year where
  show (Year n) = "Year " <> show n

instance eqYear :: Eq Year where
  eq (Year x) (Year y) = x == y

instance ordYear :: Ord Year where
  compare (Year x) (Year y) = compare x y

instance semiringYear :: Semiring Year where
  add (Year x) (Year y) = Year (x + y)
  mul (Year x) (Year y) = Year (x * y)
  zero = Year zero
  one = Year one

instance ringYear :: Ring Year where
  sub (Year x) (Year y) = Year (x - y)

instance clampedYear :: Clamped Year where
  clamp (Year x) = Year $ absInt x

-- | Extract an Int from a Year
runYear :: Year -> Int
runYear (Year x) = x

-- | Smart constructor for year
asYear :: Int -> Year
asYear = Year <<< absInt

-- | Checks to see if a given year is a leap year
isLeapYear :: Year -> Boolean
isLeapYear (Year y) | y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0) = true
                     | otherwise = false

------------------Months-------------------------
-- | A month date component value.
data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

instance eqMonth :: Eq Month where
  eq January   January   = true
  eq February  February  = true
  eq March     March     = true
  eq April     April     = true
  eq May       May       = true
  eq June      June      = true
  eq July      July      = true
  eq August    August    = true
  eq September September = true
  eq October   October   = true
  eq November  November  = true
  eq December  December  = true
  eq _         _         = false

instance ordMonth :: Ord Month where
  compare = compare `on` fromEnum

instance boundedMonth :: Bounded Month where
  bottom = January
  top = December

instance boundedOrdMonth :: BoundedOrd Month

instance showMonth :: Show Month where
  show January   = "January"
  show February  = "February"
  show March     = "March"
  show April     = "April"
  show May       = "May"
  show June      = "June"
  show July      = "July"
  show August    = "August"
  show September = "September"
  show October   = "October"
  show November  = "November"
  show December  = "December"

instance enumMonth :: Enum Month where
  cardinality = Cardinality 12
  succ = defaultSucc monthToEnum monthFromEnum
  pred = defaultPred monthToEnum monthFromEnum
  toEnum = monthToEnum
  fromEnum = monthFromEnum

monthToEnum :: Int -> Maybe Month
monthToEnum 0  = Just January
monthToEnum 1  = Just February
monthToEnum 2  = Just March
monthToEnum 3  = Just April
monthToEnum 4  = Just May
monthToEnum 5  = Just June
monthToEnum 6  = Just July
monthToEnum 7  = Just August
monthToEnum 8  = Just September
monthToEnum 9  = Just October
monthToEnum 10 = Just November
monthToEnum 11 = Just December
monthToEnum _  = Nothing

monthFromEnum :: Month -> Int
monthFromEnum January   = 0
monthFromEnum February  = 1
monthFromEnum March     = 2
monthFromEnum April     = 3
monthFromEnum May       = 4
monthFromEnum June      = 5
monthFromEnum July      = 6
monthFromEnum August    = 7
monthFromEnum September = 8
monthFromEnum October   = 9
monthFromEnum November  = 10
monthFromEnum December  = 11

-- | Smart Constructor
asMonth :: Int -> Month
asMonth n = case n of
  _ | n <= 1    -> toMonth 0
    | n > 11    -> toMonth 11
    | otherwise -> toMonth (n - 1)
  where
  toMonth = fromMaybe January <<< toEnum

-- | Calculates the total days in a given month based on year
daysInMonth :: Boolean -> Month -> Int
daysInMonth leapYear m = case m of
  February -> if leapYear then 29 else 28
  April     ->  30
  June      ->  30
  September ->  30
  November  ->  30
  _         ->  31

------------------DayOfMonth---------------------
-- | A day-of-month date component value.
newtype DayOfMonth = DayOfMonth Int

instance eqDayOfMonth :: Eq DayOfMonth where
  eq (DayOfMonth x) (DayOfMonth y) = x == y

instance ordDayOfMonth :: Ord DayOfMonth where
  compare (DayOfMonth x) (DayOfMonth y) = compare x y

instance showDayOfMonth :: Show DayOfMonth where
  show (DayOfMonth x) = "DayOfMonth " <> show x

-- | Extract an Int from a DayOfMonth
runDayOfMonth :: DayOfMonth -> Int
runDayOfMonth (DayOfMonth x) = x

-- | Smart constrcutor for DayOfMonth
asDayOfMonth :: Int -> Month -> Year -> DayOfMonth
asDayOfMonth n m y = case n of
  _ | n <= 1              -> DayOfMonth 1
    | n >= lastDayOfMonth -> DayOfMonth lastDayOfMonth
    | otherwise           -> DayOfMonth n
  where
  lastDayOfMonth = daysInMonth (isLeapYear y) m

------------------DayOfWeek----------------------
-- | A day-of-week date component value.
data DayOfWeek
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday

instance eqDayOfWeek :: Eq DayOfWeek where
  eq Sunday    Sunday    = true
  eq Monday    Monday    = true
  eq Tuesday   Tuesday   = true
  eq Wednesday Wednesday = true
  eq Thursday  Thursday  = true
  eq Friday    Friday    = true
  eq Saturday  Saturday  = true
  eq _         _         = false

instance ordDayOfWeek :: Ord DayOfWeek where
  compare = compare `on` fromEnum

instance boundedDayOfWeek :: Bounded DayOfWeek where
  bottom = Sunday
  top = Saturday

instance boundedOrdDayOfWeek :: BoundedOrd DayOfWeek

instance showDayOfWeek :: Show DayOfWeek where
  show Sunday    = "Sunday"
  show Monday    = "Monday"
  show Tuesday   = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday  = "Thursday"
  show Friday    = "Friday"
  show Saturday  = "Saturday"

instance enumDayOfWeek :: Enum DayOfWeek where
  cardinality = Cardinality 7
  succ = defaultSucc dayOfWeekToEnum dayOfWeekFromEnum
  pred = defaultPred dayOfWeekToEnum dayOfWeekFromEnum
  toEnum = dayOfWeekToEnum
  fromEnum = dayOfWeekFromEnum

dayOfWeekToEnum :: Int -> Maybe DayOfWeek
dayOfWeekToEnum 0 = Just Sunday
dayOfWeekToEnum 1 = Just Monday
dayOfWeekToEnum 2 = Just Tuesday
dayOfWeekToEnum 3 = Just Wednesday
dayOfWeekToEnum 4 = Just Thursday
dayOfWeekToEnum 5 = Just Friday
dayOfWeekToEnum 6 = Just Saturday
dayOfWeekToEnum _ = Nothing

dayOfWeekFromEnum :: DayOfWeek -> Int
dayOfWeekFromEnum Sunday    = 0
dayOfWeekFromEnum Monday    = 1
dayOfWeekFromEnum Tuesday   = 2
dayOfWeekFromEnum Wednesday = 3
dayOfWeekFromEnum Thursday  = 4
dayOfWeekFromEnum Friday    = 5
dayOfWeekFromEnum Saturday  = 6

-- | Smart Constructor
asDayOfWeek :: Int -> DayOfWeek
asDayOfWeek n = case n of
  _ | n <= 1    -> toDayOfWeek 0
    | n > 6     -> toDayOfWeek 6
    | otherwise -> toDayOfWeek (n - 1)
  where
  toDayOfWeek = fromMaybe Sunday <<< toEnum

--------------------Days-------------------------
-- | A quantity of hours (not necessarily a value between 0 and 23).
newtype Days = Days Number

instance showDays :: Show Days where
  show (Days x) = "Days " <> show x

instance eqDays :: Eq Days where
  eq (Days x) (Days y) = x == y

instance ordDays :: Ord Days where
  compare (Days x) (Days y) = compare x y

instance semiringDays :: Semiring Days where
  add (Days x) (Days y) = Days (x + y)
  mul (Days x) (Days y) = Days (x * y)
  zero = Days 0.0
  one = Days 1.0

instance ringDays :: Ring Days where
  sub (Days x) (Days y) = Days (x - y)

instance moduloSemiringDays :: ModuloSemiring Days where
  div (Days x) (Days y) = Days (x / y)
  mod _ _ = Days 0.0

instance divisionRingDays :: DivisionRing Days

instance numDays :: Num Days

-- | Smart constructor
days :: Number -> Days
days = Days

------------------HourOfDay----------------------
-- | An hour component from a time value. Should fall between 0 and 23
-- | inclusive.
newtype HourOfDay = HourOfDay Int

instance showHourOfDay :: Show HourOfDay where
  show (HourOfDay x) = "HourOfDay " <> show x

instance clampedHourOfDay :: Clamped HourOfDay where
  clamp (HourOfDay x) = case absInt x of
    x' | x' >= 23  -> HourOfDay 23
       | otherwise -> HourOfDay x'

instance eqHourOfDay :: Eq HourOfDay where
  eq (HourOfDay x) (HourOfDay y) = x == y

instance ordHourOfDay :: Ord HourOfDay where
  compare (HourOfDay x) (HourOfDay y) = compare x y

-- | Smart Constructor
asHourOfDay :: Int -> HourOfDay
asHourOfDay = clamp <<< HourOfDay

--------------------Hours------------------------
-- | A quantity of hours (not necessarily a value between 0 and 23).
newtype Hours = Hours Number

instance showHours :: Show Hours where
  show (Hours x) = "Hours " <> show x

instance eqHours :: Eq Hours where
  eq (Hours x) (Hours y) = x == y

instance ordHours :: Ord Hours where
  compare (Hours x) (Hours y) = compare x y

instance semiringHours :: Semiring Hours where
  add (Hours x) (Hours y) = Hours (x + y)
  mul (Hours x) (Hours y) = Hours (x * y)
  zero = Hours 0.0
  one = Hours 1.0

instance ringHours :: Ring Hours where
  sub (Hours x) (Hours y) = Hours (x - y)

instance moduloSemiringHours :: ModuloSemiring Hours where
  div (Hours x) (Hours y) = Hours (x / y)
  mod _ _ = Hours 0.0

instance divisionRingHours :: DivisionRing Hours

instance numHours :: Num Hours

-- | Smart constructor
hours :: Number -> Hours
hours = Hours

--------------------MinuteOfHour-----------------
-- | A minute component from a time value. Should fall between 0 and 59
-- | inclusive.
newtype MinuteOfHour = MinuteOfHour Int

instance showMinuteOfHour :: Show MinuteOfHour where
  show (MinuteOfHour x) = "MinuteOfHour " <> show x

instance clampedMinuteOfHour :: Clamped MinuteOfHour where
  clamp (MinuteOfHour x) = case absInt x of
    x' | x' >= 59  -> MinuteOfHour 59
       | otherwise -> MinuteOfHour x'

instance eqMinuteOfHour :: Eq MinuteOfHour where
  eq (MinuteOfHour x) (MinuteOfHour y) = x == y

instance ordMinuteOfHour :: Ord MinuteOfHour where
  compare (MinuteOfHour x) (MinuteOfHour y) = compare x y

-- | Smart Constructor
asMinuteOfHour :: Int -> MinuteOfHour
asMinuteOfHour = clamp <<< MinuteOfHour


--------------------Minutes----------------------
-- | A quantity of minutes (not necessarily a value between 0 and 60).
newtype Minutes = Minutes Number

instance showMinutes :: Show Minutes where
  show (Minutes x) = "Minutes " <> show x

instance eqMinutes :: Eq Minutes where
  eq (Minutes x) (Minutes y) = x == y

instance ordMinutes :: Ord Minutes where
  compare (Minutes x) (Minutes y) = compare x y

instance semiringMinutes :: Semiring Minutes where
  add (Minutes x) (Minutes y) = Minutes (x + y)
  mul (Minutes x) (Minutes y) = Minutes (x * y)
  zero = Minutes 0.0
  one = Minutes 1.0

instance ringMinutes :: Ring Minutes where
  sub (Minutes x) (Minutes y) = Minutes (x - y)

instance moduloSemiringMinutes :: ModuloSemiring Minutes where
  div (Minutes x) (Minutes y) = Minutes (x / y)
  mod _ _ = Minutes 0.0

instance divisionRingMinutes :: DivisionRing Minutes

instance numMinutes :: Num Minutes

-- | Smart Constructor
minutes :: Number -> Minutes
minutes = Minutes


--------------------SecondOfMinute---------------
-- | A second component from a time value. Should fall between 0 and 59
-- | inclusive.
newtype SecondOfMinute = SecondOfMinute Int

instance showSecondOfMinute :: Show SecondOfMinute where
  show (SecondOfMinute x) = "SecondOfMinute " <> show x

instance clampedSecondOfMinute :: Clamped SecondOfMinute where
  clamp (SecondOfMinute x) = case absInt x of
    x' | x' >= 59  -> SecondOfMinute 59
       | otherwise -> SecondOfMinute x'

instance eqSecondOfMinute :: Eq SecondOfMinute where
  eq (SecondOfMinute x) (SecondOfMinute y) = x == y

instance ordSecondOfMinute :: Ord SecondOfMinute where
  compare (SecondOfMinute x) (SecondOfMinute y) = compare x y

-- | Smart Constructor
asSecondOfMinute :: Int -> SecondOfMinute
asSecondOfMinute = clamp <<< SecondOfMinute


--------------------Seconds------------------------
-- | A quantity of seconds (not necessarily a value between 0 and 60).
newtype Seconds = Seconds Number

instance showSeconds :: Show Seconds where
  show (Seconds x) = "Seconds " <> show x

instance eqSeconds :: Eq Seconds where
  eq (Seconds x) (Seconds y) = x == y

instance ordSeconds :: Ord Seconds where
  compare (Seconds x) (Seconds y) = compare x y

instance semiringSeconds :: Semiring Seconds where
  add (Seconds x) (Seconds y) = Seconds (x + y)
  mul (Seconds x) (Seconds y) = Seconds (x * y)
  zero = Seconds 0.0
  one = Seconds 1.0

instance ringSeconds :: Ring Seconds where
  sub (Seconds x) (Seconds y) = Seconds (x - y)

instance moduloSemiringSeconds :: ModuloSemiring Seconds where
  div (Seconds x) (Seconds y) = Seconds (x / y)
  mod _ _ = Seconds 0.0

instance divisionRingSeconds :: DivisionRing Seconds

instance numSeconds :: Num Seconds

-- | Smart constructor
seconds :: Number -> Seconds
seconds = Seconds


----------------MillisecondOfSecond----------------
-- | A millisecond component from a time value. Should fall between 0 and 999
-- | inclusive.
newtype MillisecondOfSecond = MillisecondOfSecond Int

instance showMillisecondOfSecond :: Show MillisecondOfSecond where
  show (MillisecondOfSecond x) = "MillisecondOfSecond " <> show x

instance clampedMillisecondOfSecond :: Clamped MillisecondOfSecond where
  clamp (MillisecondOfSecond x) = case absInt x of
    x' | x' >= 999  -> MillisecondOfSecond 999
       | otherwise  -> MillisecondOfSecond x'

instance eqMillisecondOfSecond :: Eq MillisecondOfSecond where
  eq (MillisecondOfSecond x) (MillisecondOfSecond y) = x == y

instance ordMillisecondOfSecond :: Ord MillisecondOfSecond where
  compare (MillisecondOfSecond x) (MillisecondOfSecond y) = compare x y

-- | Smart Constructor
asMillisecondOfSecond :: Int -> MillisecondOfSecond
asMillisecondOfSecond = clamp <<< MillisecondOfSecond


---------------------Milliseconds----------------
-- | A quantity of milliseconds (not necessarily a value between 0 and 1000).
newtype Milliseconds = Milliseconds Number

instance eqMilliseconds :: Eq Milliseconds where
  eq (Milliseconds x) (Milliseconds y) = x == y

instance ordMilliseconds :: Ord Milliseconds where
  compare (Milliseconds x) (Milliseconds y) = compare x y

instance semiringMilliseconds :: Semiring Milliseconds where
  add (Milliseconds x) (Milliseconds y) = Milliseconds (x + y)
  mul (Milliseconds x) (Milliseconds y) = Milliseconds (x * y)
  zero = Milliseconds 0.0
  one = Milliseconds 1.0

instance ringMilliseconds :: Ring Milliseconds where
  sub (Milliseconds x) (Milliseconds y) = Milliseconds (x - y)

instance moduloSemiringMilliseconds :: ModuloSemiring Milliseconds where
  div (Milliseconds x) (Milliseconds y) = Milliseconds (x / y)
  mod _ _ = Milliseconds 0.0

instance divisionRingMilliseconds :: DivisionRing Milliseconds

instance numMilliseconds :: Num Milliseconds

instance showMilliseconds :: Show Milliseconds where
  show (Milliseconds n) = "Milliseconds " <> show n

-- | Smart constructor
ms :: Number -> Milliseconds
ms = Milliseconds


------------------Time Conversions---------------
class TimeValue a where
  toDays :: a -> Days
  toHours :: a -> Hours
  toMinutes :: a -> Minutes
  toSeconds :: a -> Seconds
  toMilliseconds :: a -> Milliseconds
  fromDays :: Days -> a
  fromHours :: Hours -> a
  fromMinutes :: Minutes -> a
  fromSeconds :: Seconds -> a
  fromMilliseconds :: Milliseconds -> a

instance timeValueHours :: TimeValue Hours where
  toDays (Hours n) = Days (n / 24.0)
  toHours n = n
  toMinutes (Hours n) = Minutes (n * 60.0)
  toSeconds (Hours n) = Seconds (n * 3600.0)
  toMilliseconds (Hours n) = Milliseconds (n * 3600000.0)
  fromDays (Days n) = Hours (n * 24.0)
  fromHours n = n
  fromMinutes (Minutes n) = Hours (n / 60.0)
  fromSeconds (Seconds n) = Hours (n / 3600.0)
  fromMilliseconds (Milliseconds n) = Hours (n / 3600000.0)

instance timeValueMinutes :: TimeValue Minutes where
  toDays (Minutes n) = Days (n / 1440.0)
  toHours (Minutes n) = Hours (n / 60.0)
  toMinutes n = n
  toSeconds (Minutes n) = Seconds (n * 60.0)
  toMilliseconds (Minutes n) = Milliseconds (n * 60000.0)
  fromDays (Days n) = Minutes (n * 1440.0)
  fromHours (Hours n) = Minutes (n * 60.0)
  fromMinutes n = n
  fromSeconds (Seconds n) = Minutes (n / 60.0)
  fromMilliseconds (Milliseconds n) = Minutes (n / 60000.0)

instance timeValueSeconds :: TimeValue Seconds where
  toDays (Seconds n) = Days (n / 86400.0)
  toHours (Seconds n) = Hours (n / 3600.0)
  toMinutes (Seconds n) = Minutes (n / 60.0)
  toSeconds n = n
  toMilliseconds (Seconds n) = Milliseconds (n * 1000.0)
  fromDays (Days n) = Seconds (n * 86400.0)
  fromHours (Hours n) = Seconds (n * 3600.0)
  fromMinutes (Minutes n) = Seconds (n * 60.0)
  fromSeconds n = n
  fromMilliseconds (Milliseconds n) = Seconds (n / 1000.0)

instance timeValueMilliseconds :: TimeValue Milliseconds where
  toDays (Milliseconds n) = Days (n / 86400000.0)
  toHours (Milliseconds n) = Hours (n / 3600000.0)
  toMinutes (Milliseconds n) = Minutes (n / 60000.0)
  toSeconds (Milliseconds n) = Seconds (n / 1000.0)
  toMilliseconds n = n
  fromDays (Days n) = Milliseconds (n * 86400000.0)
  fromHours (Hours n) = Milliseconds (n * 3600000.0)
  fromMinutes (Minutes n) = Milliseconds (n * 60000.0)
  fromSeconds (Seconds n) = Milliseconds (n * 1000.0)
  fromMilliseconds n = n
