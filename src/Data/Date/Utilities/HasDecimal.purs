module Data.Date.Utilities.HasDecimal
  ( HasDecimal, intPart, decPart,getNumber
  , getDecPart, getIntPart
  , getDecimal, getInt
  ) where

import Prelude
import Data.Int (floor, toNumber)

-- | Allows a type which can be represented as a Number to be split at the decimal point
class HasDecimal a where
  intPart :: a -> a
  decPart :: a -> a
  getNumber :: a -> Number


-- | For building instances
-- | Extracts just the decimal component of a number
getDecPart :: Number -> Number
getDecPart n = n `sub` toNumber (floor n)

-- | Convenience re-export of floor for numbers
getIntPart :: Number -> Number
getIntPart = toNumber <<< floor

-- | For using instances
-- | Gets the decimal number value out of a HasDecimal value
getDecimal :: forall a. (HasDecimal a) => a -> Number
getDecimal = getNumber <<< decPart

-- | Gets the integer value out of a HasDecimal value
getInt :: forall a. (HasDecimal a) => a -> Int
getInt = floor <<< getNumber
