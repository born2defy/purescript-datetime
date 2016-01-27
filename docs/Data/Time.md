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
HasDecimal Days
Semiring Days
Ring Days
ModuloSemiring Days
DivisionRing Days
Num Days
TimeValue Days
```

#### `asDays`

``` purescript
asDays :: Number -> Days
```

Smart constructor

#### `runDays`

``` purescript
runDays :: Days -> Number
```

Extractor

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

#### `runHourOfDay`

``` purescript
runHourOfDay :: HourOfDay -> Int
```

Extractor

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
HasDecimal Hours
Semiring Hours
Ring Hours
ModuloSemiring Hours
DivisionRing Hours
Num Hours
TimeValue Hours
```

#### `asHours`

``` purescript
asHours :: Number -> Hours
```

Smart constructor

#### `runHours`

``` purescript
runHours :: Hours -> Number
```

Extractor

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

#### `runMinuteOfHour`

``` purescript
runMinuteOfHour :: MinuteOfHour -> Int
```

Extractor

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
HasDecimal Minutes
Semiring Minutes
Ring Minutes
ModuloSemiring Minutes
DivisionRing Minutes
Num Minutes
TimeValue Minutes
```

#### `asMinutes`

``` purescript
asMinutes :: Number -> Minutes
```

Smart Constructor

#### `runMinutes`

``` purescript
runMinutes :: Minutes -> Number
```

Extractor

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

#### `runSecondOfMinute`

``` purescript
runSecondOfMinute :: SecondOfMinute -> Int
```

Extractor

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
HasDecimal Seconds
Semiring Seconds
Ring Seconds
ModuloSemiring Seconds
DivisionRing Seconds
Num Seconds
TimeValue Seconds
```

#### `asSeconds`

``` purescript
asSeconds :: Number -> Seconds
```

Smart constructor

#### `runSeconds`

``` purescript
runSeconds :: Seconds -> Number
```

Extractor

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

#### `runMillisecondOfSecond`

``` purescript
runMillisecondOfSecond :: MillisecondOfSecond -> Int
```

Extractor

#### `Milliseconds`

``` purescript
newtype Milliseconds
  = Milliseconds Number
```

##### Instances
``` purescript
Eq Milliseconds
Ord Milliseconds
HasDecimal Milliseconds
Semiring Milliseconds
Ring Milliseconds
ModuloSemiring Milliseconds
DivisionRing Milliseconds
Num Milliseconds
Show Milliseconds
TimeValue Milliseconds
```

#### `asMilliseconds`

``` purescript
asMilliseconds :: Number -> Milliseconds
```

Smart constructor

#### `runMilliseconds`

``` purescript
runMilliseconds :: Milliseconds -> Number
```

Extractor

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


