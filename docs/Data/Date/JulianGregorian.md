## Module Data.Date.JulianGregorian

#### `calcTaxEscrows`

``` purescript
calcTaxEscrows :: GregorianDate -> Int
```

#### `monthAsInt`

``` purescript
monthAsInt :: Month -> Int
```

Converts an Month into its traditional numeric form

#### `JulianTime`

``` purescript
newtype JulianTime
  = JulianTime Number
```

Julian Day.
The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
The plan is to start with Gregorian since it is easy to enter, then convert to Julian for any date calculations.

##### Instances
``` purescript
Show JulianTime
Eq JulianTime
Ord JulianTime
```

#### `runJulianTime`

``` purescript
runJulianTime :: JulianTime -> Number
```

#### `addDaysJT`

``` purescript
addDaysJT :: Int -> JulianTime -> JulianTime
```

#### `diffInDays`

``` purescript
diffInDays :: JulianTime -> JulianTime -> Int
```

#### `diffInDaysGregorian`

``` purescript
diffInDaysGregorian :: GregorianDate -> GregorianDate -> Int
```

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

#### `gregorianToJulianTime`

``` purescript
gregorianToJulianTime :: GregorianDate -> JulianTime
```

#### `diffGDates`

``` purescript
diffGDates :: GregorianDate -> GregorianDate -> Int
```

#### `showJulianTimeAsDateAndTime`

``` purescript
showJulianTimeAsDateAndTime :: TimeZoneOffset -> JulianTime -> String
```

#### `TimeOfDay`

``` purescript
type TimeOfDay = { hours :: HourOfDay, minutes :: MinuteOfHour, seconds :: SecondOfMinute, milliseconds :: MillisecondOfSecond }
```

A type representing the time of day

#### `showTimeOfDay`

``` purescript
showTimeOfDay :: TimeOfDay -> String
```

#### `TimeZoneOffset`

``` purescript
type TimeZoneOffset = Int
```

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

#### `showTimeOfDayStd`

``` purescript
showTimeOfDayStd :: TimeStdFmt -> String
```

#### `GregorianDate`

``` purescript
type GregorianDate = { month :: Month, day :: DayOfMonth, year :: Year }
```

#### `showGregorianDate`

``` purescript
showGregorianDate :: GregorianDate -> String
```

#### `mkGregorianDate`

``` purescript
mkGregorianDate :: Int -> Int -> Int -> GregorianDate
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

#### `monthLengths`

``` purescript
monthLengths :: Boolean -> List Int
```

#### `clip`

``` purescript
clip :: forall t. (Ord t) => t -> t -> t -> t
```

#### `gregorianToDate`

``` purescript
gregorianToDate :: forall e. GregorianDate -> Eff (now :: Now, locale :: Locale | e) Date
```

#### `dateToGregorian`

``` purescript
dateToGregorian :: Date -> GregorianDate
```

#### `dateToJulianTime`

``` purescript
dateToJulianTime :: Date -> JulianTime
```

#### `julianTimeToDate`

``` purescript
julianTimeToDate :: JulianTime -> Maybe Date
```

#### `prettyJulianTime`

``` purescript
prettyJulianTime :: JulianTime -> String
```

#### `msToDayInt`

``` purescript
msToDayInt :: Number -> Int
```

#### `dayIntToMS`

``` purescript
dayIntToMS :: Int -> Number
```

#### `runMS`

``` purescript
runMS :: Milliseconds -> Number
```


