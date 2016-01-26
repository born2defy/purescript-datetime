module Data.Date.NativeDate
  ( JSDate()
  , Date()
  , fromJSDate, toJSDate
  , fromEpochMilliseconds, fromEpochMillisecondsUnsafe, toEpochMilliseconds, withEpochMS
  , fromString, fromStringStrict
  , addDays, subtractDays, daysToMS
  , prettyDate
  , setDate, setMonth
  , beginningOfMonth, endOfMonth
  , getDayOfMonth, getDay, getMonth, getYear
  , getDateString
  , Now()
  , now, getCurrentYear, getCurrentMonth, getCurrentDay
  , nowEpochMilliseconds
  , LocaleOffset(..), timezoneOffset
  ) where

import Prelude

import Control.Monad.Eff
import Data.Function (on, Fn2(), runFn2, Fn3(), runFn3)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time
import Data.Int (toNumber)
import Data.Date.Utilities.Clamped (Clamped)

foreign import nowImpl :: forall e. (JSDate -> Date) -> Eff (now :: Now | e) Date
foreign import jsDateConstructor :: forall a. a -> JSDate
foreign import jsDateMethod :: forall a. Fn2 String JSDate a
foreign import jsDateMethodArg :: forall a b. Fn3 String JSDate a b
foreign import strictJsDate :: Fn3 (forall a. a -> Maybe a) (forall a. Maybe a) String (Maybe JSDate)

------------------Native Date--------------------
-- | A native JavaScript `Date` object.
foreign import data JSDate :: *

-- | A combined date/time value. `Date`s cannot be constructed directly to
-- | ensure they are not the `Invalid Date` value, and instead must be created
-- | via `fromJSDate`, `fromEpochMilliseconds`, `fromString`, etc. or the `date`
-- | and `dateTime` functions in the `Data.Date.Locale` and `Data.Date.UTC`
-- | modules.
newtype Date = DateTime JSDate

instance eqDate :: Eq Date where
  eq = eq `on` toEpochMilliseconds

instance ordDate :: Ord Date where
  compare = compare `on` toEpochMilliseconds

instance showDate :: Show Date where
  show d = "fromEpochMilliseconds " <> show (toEpochMilliseconds d)

-- | Shifts a Date to the end of the current month
endOfMonth ::  Date -> Date
endOfMonth date = setDate (monthDays date) date
  where
  monthDays date = daysInMonth (isLeapYear $ getYear date) (getMonth date)

-- | The native JavaScript setDate function for Dates
setDate :: Int -> Date -> Date
setDate n date =  fromMaybe date <<< fromEpochMilliseconds <<< withJSDateMethodMS "setDate" date $ n

-- | The native JavaScript setMonth function for Dates
setMonth :: Int -> Date -> Date
setMonth n date =  fromMaybe date <<< fromEpochMilliseconds <<< withJSDateMethodMS "setMonth" date $ n

-- | Shifts a Date to the beginning of the current month
beginningOfMonth :: Date -> Date
beginningOfMonth = setDate 1

-- | Gets the Day of the week of a Date using the native JavaScript Date method
getDay :: Date -> DayOfWeek
getDay = fromMaybe Sunday <<< dayOfWeekToEnum <<< withJSDateMethodInt "getDay"

-- | Gets the day of the month using the native JavaScript getDate method
getDayOfMonth :: Date -> Int
getDayOfMonth = withJSDateMethodInt "getDate"

-- | Get the date as a string using the native JavaScript toString method
getDateString :: Date -> String
getDateString = withJSDateMethodString "toString"

-- | Gets the Month of a Date using the native JavaScript Date method
getMonth :: Date -> Month
getMonth = fromMaybe January <<< monthToEnum <<< withJSDateMethodInt "getMonth"

-- | Gets the Year of a Date using the native JavaScript Date method
getYear :: Date -> Year
getYear = Year <<< withJSDateMethodInt "getFullYear"

-- | Pretty prints a Date
prettyDate :: Date -> String
prettyDate d = go getDay getMonth getDayOfMonth getYear
  where
  go day month date year = fmt day <> ", " <> fmt month <> " " <> fmt date <> ", " <> fmtYr year
  fmt :: forall a. (Show a) => (Date -> a) -> String
  fmt = show <<< ($ d)
  fmtYr = show <<< runYear <<< ($ d)

-- | Runs native JavaScript Date methods that return Ints
withJSDateMethodInt :: String -> Date -> Int
withJSDateMethodInt s = runFn2 jsDateMethod s <<< toJSDate

-- | Runs native JavaScript Date methods that return a String
withJSDateMethodString :: String -> Date -> String
withJSDateMethodString s = runFn2 jsDateMethod s <<< toJSDate

-- | Runs native JavaScript Date methods that return MS
withJSDateMethodMS :: String -> Date -> Int -> Milliseconds
withJSDateMethodMS s d n = runFn3 jsDateMethodArg s (toJSDate d) n

-- | Creates a `Date` value from a number of milliseconds elapsed since 1st
-- | January 1970 00:00:00 UTC.
fromEpochMillisecondsUnsafe :: Milliseconds -> Date
fromEpochMillisecondsUnsafe = DateTime <<< jsDateConstructor

-- | Attempts to create a `Date` from a `JSDate`. If the `JSDate` is an invalid
-- | date `Nothing` is returned.
fromJSDate :: JSDate -> Maybe Date
fromJSDate d =
  if Global.isNaN (runFn2 jsDateMethod "getTime" d)
  then Nothing
  else Just (DateTime d)

-- | Extracts a `JSDate` from a `Date`.
toJSDate :: Date -> JSDate
toJSDate (DateTime d) = d

-- | Creates a `Date` value from a number of milliseconds elapsed since 1st
-- | January 1970 00:00:00 UTC.
fromEpochMilliseconds :: Milliseconds -> Maybe Date
fromEpochMilliseconds = fromJSDate <<< jsDateConstructor

-- | Gets the number of milliseconds elapsed since 1st January 1970 00:00:00
-- | UTC for a `Date`.
toEpochMilliseconds :: Date -> Milliseconds
toEpochMilliseconds (DateTime d) = runFn2 jsDateMethod "getTime" d

-- | Attempts to construct a date from a string value using JavaScript’s
-- | [Date.parse() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse).
-- | `Nothing` is returned if the parse fails or the resulting date is invalid.
fromString :: String -> Maybe Date
fromString = fromJSDate <<< jsDateConstructor

-- | Attempts to construct a date from a simplified extended ISO 8601 format
-- | (`YYYY-MM-DDTHH:mm:ss.sssZ`). `Nothing` is returned if the format is not
-- | an exact match or the resulting date is invalid.
fromStringStrict :: String -> Maybe Date
fromStringStrict s = runFn3 strictJsDate Just Nothing s >>= fromJSDate


---------------------Now-------------------------
-- | Effect type for when accessing the current date/time.
foreign import data Now :: !

-- | Gets a `Date` value for the current date/time according to the current
-- | machine’s local time.
now :: forall e. Eff (now :: Now | e) Date
now = nowImpl DateTime

-- | Gets the current year from the local machine
getCurrentYear :: forall e. Eff (now :: Now | e) Year
getCurrentYear = now >>= getYear >>> pure

-- | Gets the current month from the local machine
getCurrentMonth :: forall e. Eff (now :: Now | e) Month
getCurrentMonth = now >>= getMonth >>> pure

-- | Gets the current day from the local machine
getCurrentDay :: forall e. Eff (now :: Now | e) DayOfWeek
getCurrentDay = now >>= getDay >>> pure

-- | Gets the number of milliseconds elapsed milliseconds since 1st January
-- | 1970 00:00:00 UTC according to the current machine’s local time
foreign import nowEpochMilliseconds :: forall e. Eff (now :: Now | e) Milliseconds

-- | A timezone locale offset, measured in minutes.
newtype LocaleOffset = LocaleOffset Minutes

-- | Get the locale time offset for a `Date`.
timezoneOffset :: Date -> LocaleOffset
timezoneOffset (DateTime d) = runFn2 jsDateMethod "getTimezoneOffset" d

-----------------Date Math-----------------------
-- | Adds a number of days to a Date
addDays :: Int -> Date -> Date
addDays n = withEpochMS $ add (daysToMS n)

-- | Subtracts a number of days from a Date
subtractDays :: Int -> Date -> Date
subtractDays n = withEpochMS $ flip sub (daysToMS n)

-- | Performs a MS -> MS transformation over a Date
withEpochMS :: (Milliseconds -> Milliseconds) -> Date -> Date
withEpochMS f d = fromMaybe d <<< fromEpochMilliseconds <<< f <<< toEpochMilliseconds $ d

-- | Converts a number of days into MS
daysToMS :: Int -> Milliseconds
daysToMS = Milliseconds <<< (* (3600000.0 * 24.0)) <<< toNumber
