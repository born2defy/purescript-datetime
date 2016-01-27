## Module Data.Date.Utilities.HasDecimal

#### `HasDecimal`

``` purescript
class HasDecimal a where
  intPart :: a -> a
  decPart :: a -> a
  getNumber :: a -> Number
```

Allows a type which can be represented as a Number to be split at the decimal point

#### `getDecPart`

``` purescript
getDecPart :: Number -> Number
```

For building instances
Extracts just the decimal component of a number

#### `getIntPart`

``` purescript
getIntPart :: Number -> Number
```

Convenience re-export of floor for numbers

#### `getDecimal`

``` purescript
getDecimal :: forall a. (HasDecimal a) => a -> Number
```

For using instances
Gets the decimal number value out of a HasDecimal value

#### `getInt`

``` purescript
getInt :: forall a. (HasDecimal a) => a -> Int
```

Gets the integer value out of a HasDecimal value


