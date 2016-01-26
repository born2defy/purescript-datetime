## Module Data.Date.JulianGregorian

#### `calcTaxEscrows`

``` purescript
calcTaxEscrows :: GregorianDate -> Int
```

#### `monthAsInt`

``` purescript
monthAsInt :: Month -> Int
```

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

#### `JulianDay`

``` purescript
newtype JulianDay
  = JulianDay Int
```

Julian Day.
The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
The plan is to start with Gregorian since it is easy to enter, then convert to Julian for any date calculations.

##### Instances
``` purescript
Show JulianDay
Eq JulianDay
Ord JulianDay
```

#### `runJulianDay`

``` purescript
runJulianDay :: JulianDay -> Int
```

#### `addJDays`

``` purescript
addJDays :: Int -> JulianDay -> JulianDay
```

#### `diffJDays`

``` purescript
diffJDays :: JulianDay -> JulianDay -> Int
```

#### `diffGDates`

``` purescript
diffGDates :: GregorianDate -> GregorianDate -> Int
```

#### `julianToOrdinal`

``` purescript
julianToOrdinal :: JulianDay -> Tuple Int Int
```

#### `ordinalToJulian`

``` purescript
ordinalToJulian :: Tuple Int Int -> JulianDay
```

#### `GregorianDate`

``` purescript
type GregorianDate = { month :: Month, day :: DayOfMonth, year :: Year }
```

#### `showGregorianDate`

``` purescript
showGregorianDate :: GregorianDate -> String
```

#### `julianToGregorian`

``` purescript
julianToGregorian :: JulianDay -> GregorianDate
```

#### `mkGregorianDate`

``` purescript
mkGregorianDate :: Int -> Int -> Int -> GregorianDate
```

#### `gregorianToJulian`

``` purescript
gregorianToJulian :: GregorianDate -> JulianDay
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

#### `dateToJulian`

``` purescript
dateToJulian :: Date -> JulianDay
```

#### `julianToDate`

``` purescript
julianToDate :: JulianDay -> Maybe Date
```

#### `prettyJulian`

``` purescript
prettyJulian :: JulianDay -> String
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


