module Data.Date
  ( Date()
  , now, getCurrentYear, getCurrentMonth, getCurrentDay
  , endOfMonth, beginningOfMonth
  , mkDate
  , calcEscrows, calcTaxEscrows
  ) where

import Prelude
import Control.Monad.Eff (Eff())
import Data.Date.JulianGregorian as JT
import Data.Time (Year(), Month(), DayOfWeek(), Month(November), advanceMonths)
import Data.Date.NativeDate as Native
import Data.Function (on)
import Data.Enum (fromEnum)

-- | By default, a `Date`is represented as a `JulianTime` value
type Date = JT.JulianTime

-- | Gets a `Date` value for the current date/time according to the current
-- | machine’s local time.
now :: forall eff. Eff (now :: Native.Now | eff) Date
now = Native.now >>= pure <<< JT.dateToJulianTime

-- | Gets the current year from the local machine
getCurrentYear :: forall eff. Eff (now :: Native.Now | eff) Year
getCurrentYear = Native.getCurrentYear

-- | Gets the current month from the local machine
getCurrentMonth :: forall eff. Eff (now :: Native.Now | eff) Month
getCurrentMonth = Native.getCurrentMonth

-- | Gets the current day from the local machine
getCurrentDay :: forall eff. Eff (now :: Native.Now | eff) DayOfWeek
getCurrentDay = Native.getCurrentDay

-- | Takes a `date` and returns a new `date` representing the last day of that month
endOfMonth :: Date -> Date
endOfMonth = asGregorian JT.endOfMonth

-- | Takes a `date` and returns a new `date` representing the first day of that month
beginningOfMonth :: Date -> Date
beginningOfMonth = asGregorian JT.beginningOfMonth

-- | Construct a `date` from the numeric representation month day year
mkDate :: Int -> Int -> Int -> Date
mkDate = JT.julianTimeSimple

-- | Calculate the number of months needed for escrows
calcEscrows :: Month -> Date -> Int
calcEscrows monthDue closingDate
  = let gd = JT.julianTimeToGregorian closingDate
        pmtMonth = advanceMonths 2 gd.month
        subMonth m1 = (+ 1) <<< (sub `on` fromEnum) m1
    in  case pmtMonth of
      _  | pmtMonth <  monthDue -> 14 - (monthDue `subMonth` pmtMonth)
         | pmtMonth == monthDue -> 14
         | otherwise            -> 2


-- | Calculate the number of months needed for escrows
calcTaxEscrows :: Date -> Int
calcTaxEscrows = calcEscrows November

asGregorian :: (JT.GregorianDate -> JT.GregorianDate) -> (Date -> Date)
asGregorian f = JT.gregorianToJulianTime <<< f <<< JT.julianTimeToGregorian
