## Module Data.Date.JulianGregorian

#### `JulianTime`

``` purescript
newtype JulianTime
  = JulianTime Number
```

##### Instances
``` purescript
Show JulianTime
HasDecimal JulianTime
Eq JulianTime
Ord JulianTime
```

#### `runJulianTime`

``` purescript
runJulianTime :: JulianTime -> Number
```

Extract a number from a JulianTime value

#### `addDaysJT`

``` purescript
addDaysJT :: Int -> JulianTime -> JulianTime
```

Add a number of days to a JulianTime value

#### `diffInDays`

``` purescript
diffInDays :: JulianTime -> JulianTime -> Int
```

Get the difference in days between two JulianTime values

#### `getDayOfWeek`

``` purescript
getDayOfWeek :: JulianTime -> DayOfWeek
```

Get the DayOfWeek of a JulianTime value

#### `firstJulianDay`

``` purescript
firstJulianDay :: DayOfWeek
```

The DayOfWeek of the first Julian Day (day 0)

#### `prettyJulianTime`

``` purescript
prettyJulianTime :: JulianTime -> String
```

Pretty print a JulianTime value

#### `julianTimeSimple`

``` purescript
julianTimeSimple :: Int -> Int -> Int -> JulianTime
```

Create a JulianTime value from a Month Date and Year

#### `TimeOfDay`

``` purescript
type TimeOfDay = { hours :: HourOfDay, minutes :: MinuteOfHour, seconds :: SecondOfMinute, milliseconds :: MillisecondOfSecond }
```

#### `showTimeOfDay`

``` purescript
showTimeOfDay :: TimeOfDay -> String
```

Display a TimeOfDay value

#### `TimeZoneOffset`

``` purescript
type TimeZoneOffset = Int
```

The time zone offset

#### `toTimeOfDay`

``` purescript
toTimeOfDay :: TimeZoneOffset -> Days -> TimeOfDay
```

Takes a partial day and converts it into a time of day

#### `AMPM`

``` purescript
data AMPM
  = AM
  | PM
```

AM or PM

##### Instances
``` purescript
Show AMPM
```

#### `TimeOfDayString`

``` purescript
type TimeOfDayString = { hoursStr :: String, minutesStr :: String, secondsStr :: String, millisecondsStr :: String }
```

A type for displaying the time of day

#### `TimeStdFmt`

``` purescript
type TimeStdFmt = { time :: TimeOfDayString, ampm :: String }
```

#### `timeToStdFmt`

``` purescript
timeToStdFmt :: TimeOfDay -> TimeStdFmt
```

Convert a TimeOfDay value to a TimeStdFmt.  This is useful for pretty printing.

#### `showTimeOfDayStd`

``` purescript
showTimeOfDayStd :: TimeStdFmt -> String
```

One way of displaying the TimeOfDay

#### `getTimeOfDay`

``` purescript
getTimeOfDay :: TimeZoneOffset -> JulianTime -> TimeOfDay
```

Get the TimeOfDay from a JulianTime value

#### `showJulianTimeAsDateAndTime`

``` purescript
showJulianTimeAsDateAndTime :: TimeZoneOffset -> JulianTime -> String
```

Display a JulianTime value as a Time and Date

#### `GregorianDate`

``` purescript
type GregorianDate = { month :: Month, day :: DayOfMonth, year :: Year }
```

#### `showGregorianDate`

``` purescript
showGregorianDate :: GregorianDate -> String
```

Show a Gregorian Date

#### `gregorianDate`

``` purescript
gregorianDate :: Int -> Int -> Int -> GregorianDate
```

Smart constructor for Gregorian Dates

#### `diffInDaysGregorian`

``` purescript
diffInDaysGregorian :: GregorianDate -> GregorianDate -> Int
```

Get the difference in days between two Gregorian Date values

#### `endOfMonth`

``` purescript
endOfMonth :: GregorianDate -> GregorianDate
```

Take a date and return a new date representing the last day of that month

#### `beginningOfMonth`

``` purescript
beginningOfMonth :: GregorianDate -> GregorianDate
```

Takes a `date` and returns a new `date` representing the first day of that month

#### `gregorianToDate`

``` purescript
gregorianToDate :: forall e. GregorianDate -> Eff (now :: Now, locale :: Locale | e) Date
```

#### `dateToGregorian`

``` purescript
dateToGregorian :: Date -> GregorianDate
```

Convert a Native Date to a Gregorian Date

#### `dateToJulianTime`

``` purescript
dateToJulianTime :: Date -> JulianTime
```

Convert a Native Date to a JulianTime value

#### `julianTimeToDate`

``` purescript
julianTimeToDate :: JulianTime -> Maybe Date
```

Convert a JulianTime value to a Native Date value

#### `julianTimeToOrdinal`

``` purescript
julianTimeToOrdinal :: JulianTime -> Tuple Int Int
```

#### `ordinalToJulianTime`

``` purescript
ordinalToJulianTime :: Tuple Int Int -> JulianTime
```

#### `julianTimeToGregorian`

``` purescript
julianTimeToGregorian :: JulianTime -> GregorianDate
```

Convert a JulianTime value to a Gregorian Date.

#### `gregorianToJulianTime`

``` purescript
gregorianToJulianTime :: GregorianDate -> JulianTime
```

#### `monthAndDayToDayOfYear`

``` purescript
monthAndDayToDayOfYear :: Boolean -> Int -> Int -> Int
```

#### `dayOfYearToMonthAndDay`

``` purescript
dayOfYearToMonthAndDay :: Boolean -> Int -> Tuple Int Int
```

#### `findMonthDay`

``` purescript
findMonthDay :: List Int -> Int -> Tuple Int Int
```

Calculate the month and day

#### `monthLengths`

``` purescript
monthLengths :: Boolean -> List Int
```

Calculate the length of a month

#### `clip`

``` purescript
clip :: forall t. (Ord t) => t -> t -> t -> t
```


