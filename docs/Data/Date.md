## Module Data.Date

#### `Date`

``` purescript
type Date = JulianTime
```

By default, a `Date`is represented as a `JulianTime` value

#### `now`

``` purescript
now :: forall eff. Eff (now :: Now | eff) Date
```

Gets a `Date` value for the current date/time according to the current
machine’s local time.

#### `getCurrentYear`

``` purescript
getCurrentYear :: forall eff. Eff (now :: Now | eff) Year
```

Gets the current year from the local machine

#### `getCurrentMonth`

``` purescript
getCurrentMonth :: forall eff. Eff (now :: Now | eff) Month
```

Gets the current month from the local machine

#### `getCurrentDay`

``` purescript
getCurrentDay :: forall eff. Eff (now :: Now | eff) DayOfWeek
```

Gets the current day from the local machine

#### `endOfMonth`

``` purescript
endOfMonth :: Date -> Date
```

Takes a `date` and returns a new `date` representing the last day of that month

#### `beginningOfMonth`

``` purescript
beginningOfMonth :: Date -> Date
```

Takes a `date` and returns a new `date` representing the first day of that month

#### `mkDate`

``` purescript
mkDate :: Int -> Int -> Int -> Date
```

Construct a `date` from the numeric representation month day year

#### `calcEscrows`

``` purescript
calcEscrows :: Month -> Date -> Int
```

Calculate the number of months needed for escrows

#### `calcTaxEscrows`

``` purescript
calcTaxEscrows :: Date -> Int
```

Calculate the number of months needed for escrows


