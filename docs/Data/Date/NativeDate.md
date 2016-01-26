## Module Data.Date.NativeDate

#### `JSDate`

``` purescript
data JSDate :: *
```

#### `Date`

``` purescript
newtype Date
```

A combined date/time value. `Date`s cannot be constructed directly to
ensure they are not the `Invalid Date` value, and instead must be created
via `fromJSDate`, `fromEpochMilliseconds`, `fromString`, etc. or the `date`
and `dateTime` functions in the `Data.Date.Locale` and `Data.Date.UTC`
modules.

##### Instances
``` purescript
Eq Date
Ord Date
Show Date
```

#### `endOfMonth`

``` purescript
endOfMonth :: Date -> Date
```

Shifts a Date to the end of the current month

#### `setDate`

``` purescript
setDate :: Int -> Date -> Date
```

The native JavaScript setDate function for Dates

#### `setMonth`

``` purescript
setMonth :: Int -> Date -> Date
```

The native JavaScript setMonth function for Dates

#### `beginningOfMonth`

``` purescript
beginningOfMonth :: Date -> Date
```

Shifts a Date to the beginning of the current month

#### `getDay`

``` purescript
getDay :: Date -> DayOfWeek
```

Gets the Day of the week of a Date using the native JavaScript Date method

#### `getDayOfMonth`

``` purescript
getDayOfMonth :: Date -> Int
```

Gets the day of the month using the native JavaScript getDate method

#### `getDateString`

``` purescript
getDateString :: Date -> String
```

Get the date as a string using the native JavaScript toString method

#### `getMonth`

``` purescript
getMonth :: Date -> Month
```

Gets the Month of a Date using the native JavaScript Date method

#### `getYear`

``` purescript
getYear :: Date -> Year
```

Gets the Year of a Date using the native JavaScript Date method

#### `prettyDate`

``` purescript
prettyDate :: Date -> String
```

Pretty prints a Date

#### `fromEpochMillisecondsUnsafe`

``` purescript
fromEpochMillisecondsUnsafe :: Milliseconds -> Date
```

Creates a `Date` value from a number of milliseconds elapsed since 1st
January 1970 00:00:00 UTC.

#### `fromJSDate`

``` purescript
fromJSDate :: JSDate -> Maybe Date
```

Attempts to create a `Date` from a `JSDate`. If the `JSDate` is an invalid
date `Nothing` is returned.

#### `toJSDate`

``` purescript
toJSDate :: Date -> JSDate
```

Extracts a `JSDate` from a `Date`.

#### `fromEpochMilliseconds`

``` purescript
fromEpochMilliseconds :: Milliseconds -> Maybe Date
```

Creates a `Date` value from a number of milliseconds elapsed since 1st
January 1970 00:00:00 UTC.

#### `toEpochMilliseconds`

``` purescript
toEpochMilliseconds :: Date -> Milliseconds
```

Gets the number of milliseconds elapsed since 1st January 1970 00:00:00
UTC for a `Date`.

#### `fromString`

``` purescript
fromString :: String -> Maybe Date
```

Attempts to construct a date from a string value using JavaScript’s
[Date.parse() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse).
`Nothing` is returned if the parse fails or the resulting date is invalid.

#### `fromStringStrict`

``` purescript
fromStringStrict :: String -> Maybe Date
```

Attempts to construct a date from a simplified extended ISO 8601 format
(`YYYY-MM-DDTHH:mm:ss.sssZ`). `Nothing` is returned if the format is not
an exact match or the resulting date is invalid.

#### `Now`

``` purescript
data Now :: !
```

#### `now`

``` purescript
now :: forall e. Eff (now :: Now | e) Date
```

Gets a `Date` value for the current date/time according to the current
machine’s local time.

#### `getCurrentYear`

``` purescript
getCurrentYear :: forall e. Eff (now :: Now | e) Year
```

Gets the current year from the local machine

#### `getCurrentMonth`

``` purescript
getCurrentMonth :: forall e. Eff (now :: Now | e) Month
```

Gets the current month from the local machine

#### `getCurrentDay`

``` purescript
getCurrentDay :: forall e. Eff (now :: Now | e) DayOfWeek
```

Gets the current day from the local machine

#### `nowEpochMilliseconds`

``` purescript
nowEpochMilliseconds :: forall e. Eff (now :: Now | e) Milliseconds
```

Gets the number of milliseconds elapsed milliseconds since 1st January
1970 00:00:00 UTC according to the current machine’s local time

#### `LocaleOffset`

``` purescript
newtype LocaleOffset
  = LocaleOffset Minutes
```

A timezone locale offset, measured in minutes.

#### `timezoneOffset`

``` purescript
timezoneOffset :: Date -> LocaleOffset
```

Get the locale time offset for a `Date`.

#### `addDays`

``` purescript
addDays :: Int -> Date -> Date
```

#### `subtractDays`

``` purescript
subtractDays :: Int -> Date -> Date
```

Subtracts a number of days from a Date

#### `withEpochMS`

``` purescript
withEpochMS :: (Milliseconds -> Milliseconds) -> Date -> Date
```

Performs a MS -> MS transformation over a Date

#### `daysToMS`

``` purescript
daysToMS :: Int -> Milliseconds
```

Converts a number of days into MS


