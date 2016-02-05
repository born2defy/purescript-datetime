module Data.HLists.HTests where

import Data.Date.JulianGregorian
import Data.Time
import Data.Date.Utilities.HasDecimal (getInt)
import Prelude
import Data.Foldable (intercalate)
import Data.Exists
import Data.HLists.ExistsR

mkDate :: Int -> Int -> Int -> JulianTime
mkDate = julianTimeSimple

class IsAsset a where
  currentBalance :: a -> Number
  startDate :: a -> JulianTime
  endDate :: a -> JulianTime

data ChkAct = Ck Number JulianTime JulianTime

instance isAssetChkAct :: IsAsset ChkAct where
  currentBalance (Ck balance startdate enddate) = balance
  startDate (Ck balance startdate enddate) = startdate
  endDate (Ck balance startdate enddate) = enddate

data SavAct = Sv Number JulianTime JulianTime

instance isAssetSavAct :: IsAsset SavAct where
  currentBalance (Sv balance startdate enddate) = balance
  startDate (Sv balance startdate enddate) = startdate
  endDate (Sv balance startdate enddate) = enddate

newtype AssetF a r = Asset ((AssetType a r))

type AssetType a r =
  { value :: a
  , currentBalance :: a -> Number
  , startDate :: a -> JulianTime
  , endDate :: a -> JulianTime
  | r
  }

mkAssetF :: forall a. (IsAsset a) => a -> AssetF a ()
mkAssetF a = Asset
  { value: a
  , currentBalance: currentBalance
  , startDate: startDate
  , endDate: endDate
  }

instance isAssetAssetR :: IsAsset (AssetR a) where
  currentBalance (AR e) = runExistsR (\(Asset r) -> r.currentBalance r.value) e
  startDate (AR e) = runExistsR (\(Asset r)  -> r.startDate r.value) e
  endDate (AR e) = runExistsR (\(Asset r)  -> r.endDate r.value) e

newtype AssetR a = AR (ExistsR (AssetF a))

runAR :: forall a.  AssetR a -> ExistsR (AssetF a)
runAR (AR e) = e

type Asset = Exists AssetR

mkAsset :: forall a. (IsAsset a) => a -> Asset
mkAsset = mkExists <<< AR <<< mkExistsR <<< mkAssetF

assets :: Array Asset
assets = [mkAsset $ Ck 100.0 (mkDate 11 27 2015) (mkDate 12 25 2015), mkAsset $ Sv 2001.0 (mkDate 10 27 2015) (mkDate 12 21 2015)]

balances :: Array Asset -> String
balances = intercalate ", " <<< map (runExists rest)
  where
  rest :: forall a. AssetR a -> String
  rest = append "current balance: " <<< show <<< currentBalance

startDates :: Array Asset -> String
startDates = intercalate ", " <<< map (runExists rest)
  where
  rest :: forall a. AssetR a -> String
  rest = append "startDate: " <<< prettyJulianTime <<< startDate

----------Take 2 with Polymorphic Records--------

data Test a r = Test
  { a :: a
  , b :: String
  , c :: Boolean
  | r
  }

testA :: Test String (d :: Number)
testA = Test
  { a : "Happy"
  , b : "Poop"
  , c: false
  , d: 143.4
  }

testB :: Test String (e :: Int)
testB = Test
  { a : "Sad"
  , b : "Fart"
  , c: true
  , e: 15
  }

type StrRec = ExistsR (Test String)

mkStrRec :: forall r. Test String r -> StrRec
mkStrRec = mkExistsR

testes :: Array StrRec
testes = [ mkStrRec testA, mkStrRec testB ]

stuff :: forall r. Test String r -> String
stuff (Test t) = t.a <> " " <> t.b

runStuff :: Array StrRec -> String
runStuff = intercalate ", " <<< map (runExistsR stuff)

newtype TestR a = TR (ExistsR (Test a))

runTR :: forall a. TestR a -> ExistsR (Test a)
runTR (TR e) = e

type TestS = Exists TestR

mkTestS :: forall a r. Test a r -> TestS
mkTestS = mkExists <<< TR <<< mkExistsR

testA1 :: Test Int (d :: Number)
testA1 = Test
  { a : 15
  , b : "Poop"
  , c: false
  , d: 143.4
  }

testes1 :: Array TestS
testes1 = [ mkTestS testA1, mkTestS testB ]

stuff2 :: forall a r. Test a r -> String
stuff2 (Test t) = t.b <> " " <> show t.c

runTestes2 :: Array TestS -> String
runTestes2 = intercalate ", " <<< map (runExists (runExistsR stuff2 <<< runTR))

-------------------More Specific Examples--------
newtype BasicAsset r = BasicAsset
  { balance :: Number
  , firstActivity :: JulianTime
  , lastActivity :: JulianTime
  | r
  }

instance isAssetBasicAsset :: IsAsset (BasicAsset r) where
  currentBalance (BasicAsset o) = o.balance
  startDate (BasicAsset o) = o.firstActivity
  endDate (BasicAsset o) = o.lastActivity

myCheck :: BasicAsset ()
myCheck = BasicAsset
  { balance : 1750.25
  , firstActivity : mkDate 1 27 2015
  , lastActivity : mkDate 2 27 2015
  }

mySaving :: BasicAsset ()
mySaving = BasicAsset
  { balance : 3750.25
  , firstActivity : mkDate 5 27 2015
  , lastActivity : mkDate 6 27 2015
  }

newtype BasicRetirement r = BasicRetirement
  { vestedBalance :: Number
  , totalBalance :: Number
  , firstActivity :: JulianTime
  , lastActivity :: JulianTime
  | r
  }

instance isAssetBasicRetirement :: IsAsset (BasicRetirement r) where
  currentBalance (BasicRetirement o) = o.totalBalance - o.vestedBalance
  startDate (BasicRetirement o) = o.firstActivity
  endDate (BasicRetirement o) = o.lastActivity

my401k :: BasicRetirement ()
my401k = BasicRetirement
  { vestedBalance : 2500.0
  , totalBalance : 3750.0
  , firstActivity : mkDate 7 27 2015
  , lastActivity : mkDate 9 27 2015
  }

myIRA :: BasicRetirement (bank :: String)
myIRA = BasicRetirement
  { vestedBalance : 2500.0
  , totalBalance : 300750.0
  , firstActivity : mkDate 6 27 2015
  , lastActivity : mkDate 9 27 2015
  , bank : "BankOfAdam"
  }

myAssets :: Array Asset
myAssets = [ mkAsset my401k, mkAsset myCheck, mkAsset mySaving ]

class (IsAsset a) <= IsRetirement a where
  vestedBalance :: a -> Number
  totalBalance :: a -> Number

instance isRetirementBasicRetirement :: IsRetirement (BasicRetirement r) where
  vestedBalance (BasicRetirement o) = o.vestedBalance
  totalBalance (BasicRetirement o) = o.totalBalance

type RetirementFields a r = (vestedBalance :: a -> Number, totalBalance :: a -> Number | r)

newtype RetirementF a r = Retirement ((AssetType a (RetirementFields a r)))

mkRetirementF :: forall a. (IsRetirement a) => a -> RetirementF a ()
mkRetirementF a = Retirement
  { value: a
  , currentBalance: currentBalance
  , startDate: startDate
  , endDate: endDate
  , vestedBalance: vestedBalance
  , totalBalance : totalBalance
  }

newtype RetirementR a = RR (ExistsR (RetirementF a))

instance isAssetRetirementR :: IsAsset (RetirementR a) where
  currentBalance (RR e) = runExistsR (\(Retirement r) -> r.currentBalance r.value) e
  startDate (RR e) = runExistsR (\(Retirement r)  -> r.startDate r.value) e
  endDate (RR e) = runExistsR (\(Retirement r)  -> r.endDate r.value) e

instance isRetirementRetirementR :: IsRetirement (RetirementR a) where
  vestedBalance (RR e) = runExistsR (\(Retirement r) -> r.vestedBalance r.value) e
  totalBalance (RR e) = runExistsR (\(Retirement r) -> r.totalBalance  r.value) e

runRR :: forall a.  RetirementR a -> ExistsR (RetirementF a)
runRR (RR e) = e

type Retirement = Exists RetirementR

mkRetirement :: forall a. (IsRetirement a) => a -> Retirement
mkRetirement = mkExists <<< RR <<< mkExistsR <<< mkRetirementF

newtype SpecialRetirement r = SR
  { spentAmount :: Number
  , originalAmount :: Number
  , firstActivityTime :: Days
  , lastActivityTime :: Days
  , totalAmount :: Number
  , unvestedAmount :: Number
  | r
  }

instance isAssetSpecialRetirement :: IsAsset (SpecialRetirement r) where
    currentBalance (SR o) = o.originalAmount - o.spentAmount
    startDate (SR o) = addDaysJT (getInt o.firstActivityTime) $ mkDate 1 1 2015
    endDate (SR o) = addDaysJT (getInt o.lastActivityTime) $ mkDate 1 1 2015

instance isRetirementSpecialRetirement :: IsRetirement (SpecialRetirement r) where
    vestedBalance (SR o) = o.totalAmount - o.unvestedAmount
    totalBalance (SR o) = o.totalAmount

mySpecial :: SpecialRetirement (poop :: Boolean)
mySpecial = SR
  { spentAmount : 150.0
  , originalAmount : 375.0
  , firstActivityTime : asDays 30.0
  , lastActivityTime : asDays 115.7
  , totalAmount : 250000.0
  , unvestedAmount : 75000.0
  , poop : true
  }

retirementAssets :: Array Retirement
retirementAssets = [mkRetirement my401k, mkRetirement myIRA, mkRetirement mySpecial]

vestedBalances :: Array Retirement -> String
vestedBalances = intercalate ", " <<< map (runExists rest)
  where
  rest :: forall a. RetirementR a -> String
  rest = append "vested balance: " <<< show <<< vestedBalance

totalBalances :: Array Retirement -> String
totalBalances = intercalate ", " <<< map (runExists rest)
  where
  rest :: forall a. RetirementR a -> String
  rest = append "total balance: " <<< show <<< totalBalance

balancesR :: Array Retirement -> String
balancesR = intercalate ", " <<< map (runExists rest)
  where
  rest :: forall a. RetirementR a -> String
  rest = append "current balance: " <<< show <<< currentBalance

startDatesR :: Array Retirement -> String
startDatesR = intercalate ", " <<< map (runExists rest)
  where
  rest :: forall a. RetirementR a -> String
  rest = append "startDate: " <<< prettyJulianTime <<< startDate

endDatesR :: Array Retirement -> String
endDatesR = intercalate ", " <<< map (runExists rest)
  where
  rest :: forall a. RetirementR a -> String
  rest = append "endDate: " <<< prettyJulianTime <<< endDate
