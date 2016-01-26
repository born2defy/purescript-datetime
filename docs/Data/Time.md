## Module Data.Time

#### `Year`

``` purescript
newtype Year
  = Year Int
```

##### Instances
``` purescript
Show Year
Eq Year
Ord Year
Semiring Year
Ring Year
Clamped Year
```

#### `runYear`

``` purescript
runYear :: Year -> Int
```

Extract an Int from a Year

#### `asYear`

``` purescript
asYear :: Int -> Year
```

Smart constructor for year

#### `isLeapYear`

``` purescript
isLeapYear :: Year -> Boolean
```

Checks to see if a given year is a leap year

#### `Month`

``` purescript
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
```

##### Instances
``` purescript
Eq Month
Ord Month
Bounded Month
BoundedOrd Month
Show Month
Enum Month
```

#### `monthToEnum`

``` purescript
monthToEnum :: Int -> Maybe Month
```

#### `monthFromEnum`

``` purescript
monthFromEnum :: Month -> Int
```

#### `asMonth`

``` purescript
asMonth :: Int -> Month
```

Smart Constructor

#### `daysInMonth`

``` purescript
daysInMonth :: Boolean -> Month -> Int
```

Calculates the total days in a given month based on year

#### `DayOfMonth`

``` purescript
newtype DayOfMonth
  = DayOfMonth Int
```

##### Instances
``` purescript
Eq DayOfMonth
Ord DayOfMonth
Show DayOfMonth
```

#### `runDayOfMonth`

``` purescript
runDayOfMonth :: DayOfMonth -> Int
```

Extract an Int from a DayOfMonth

#### `asDayOfMonth`

``` purescript
asDayOfMonth :: Int -> Month -> Year -> DayOfMonth
```

Smart constrcutor for DayOfMonth

#### `DayOfWeek`

``` purescript
data DayOfWeek
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
```

##### Instances
``` purescript
Eq DayOfWeek
Ord DayOfWeek
Bounded DayOfWeek
BoundedOrd DayOfWeek
Show DayOfWeek
Enum DayOfWeek
```

#### `dayOfWeekToEnum`

``` purescript
dayOfWeekToEnum :: Int -> Maybe DayOfWeek
```

#### `dayOfWeekFromEnum`

``` purescript
dayOfWeekFromEnum :: DayOfWeek -> Int
```

#### `asDayOfWeek`

``` purescript
asDayOfWeek :: Int -> DayOfWeek
```

Smart Constructor

#### `Days`

``` purescript
newtype Days
  = Days Number
```

##### Instances
``` purescript
Show Days
Eq Days
Ord Days
Semiring Days
Ring Days
ModuloSemiring Days
DivisionRing Days
Num Days
TimeValue Days
```

#### `runDays`

``` purescript
runDays :: Days -> Number
```

Extract a number from a Day

#### `days`

``` purescript
days :: Number -> Days
```

Smart constructor

#### `HourOfDay`

``` purescript
newtype HourOfDay
  = HourOfDay Int
```

##### Instances
``` purescript
Show HourOfDay
Clamped HourOfDay
Eq HourOfDay
Ord HourOfDay
```

#### `asHourOfDay`

``` purescript
asHourOfDay :: Int -> HourOfDay
```

Smart Constructor

#### `Hours`

``` purescript
newtype Hours
  = Hours Number
```

##### Instances
``` purescript
Show Hours
Eq Hours
Ord Hours
Semiring Hours
Ring Hours
ModuloSemiring Hours
DivisionRing Hours
Num Hours
TimeValue Hours
```

#### `hours`

``` purescript
hours :: Number -> Hours
```

Smart constructor

#### `MinuteOfHour`

``` purescript
newtype MinuteOfHour
  = MinuteOfHour Int
```

##### Instances
``` purescript
Show MinuteOfHour
Clamped MinuteOfHour
Eq MinuteOfHour
Ord MinuteOfHour
```

#### `asMinuteOfHour`

``` purescript
asMinuteOfHour :: Int -> MinuteOfHour
```

Smart Constructor

#### `Minutes`

``` purescript
newtype Minutes
  = Minutes Number
```

##### Instances
``` purescript
Show Minutes
Eq Minutes
Ord Minutes
Semiring Minutes
Ring Minutes
ModuloSemiring Minutes
DivisionRing Minutes
Num Minutes
TimeValue Minutes
```

#### `minutes`

``` purescript
minutes :: Number -> Minutes
```

Smart Constructor

#### `SecondOfMinute`

``` purescript
newtype SecondOfMinute
  = SecondOfMinute Int
```

##### Instances
``` purescript
Show SecondOfMinute
Clamped SecondOfMinute
Eq SecondOfMinute
Ord SecondOfMinute
```

#### `asSecondOfMinute`

``` purescript
asSecondOfMinute :: Int -> SecondOfMinute
```

Smart Constructor

#### `Seconds`

``` purescript
newtype Seconds
  = Seconds Number
```

##### Instances
``` purescript
Show Seconds
Eq Seconds
Ord Seconds
Semiring Seconds
Ring Seconds
ModuloSemiring Seconds
DivisionRing Seconds
Num Seconds
TimeValue Seconds
```

#### `seconds`

``` purescript
seconds :: Number -> Seconds
```

Smart constructor

#### `MillisecondOfSecond`

``` purescript
newtype MillisecondOfSecond
  = MillisecondOfSecond Int
```

##### Instances
``` purescript
Show MillisecondOfSecond
Clamped MillisecondOfSecond
Eq MillisecondOfSecond
Ord MillisecondOfSecond
```

#### `asMillisecondOfSecond`

``` purescript
asMillisecondOfSecond :: Int -> MillisecondOfSecond
```

Smart Constructor

#### `Milliseconds`

``` purescript
newtype Milliseconds
  = Milliseconds Number
```

##### Instances
``` purescript
Eq Milliseconds
Ord Milliseconds
Semiring Milliseconds
Ring Milliseconds
ModuloSemiring Milliseconds
DivisionRing Milliseconds
Num Milliseconds
Show Milliseconds
TimeValue Milliseconds
```

#### `ms`

``` purescript
ms :: Number -> Milliseconds
```

Smart constructor

#### `TimeValue`

``` purescript
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
```

##### Instances
``` purescript
TimeValue Days
TimeValue Hours
TimeValue Minutes
TimeValue Seconds
TimeValue Milliseconds
```


