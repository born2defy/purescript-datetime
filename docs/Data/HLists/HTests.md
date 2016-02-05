## Module Data.HLists.HTests

#### `mkDate`

``` purescript
mkDate :: Int -> Int -> Int -> JulianTime
```

#### `IsAsset`

``` purescript
class IsAsset a where
  currentBalance :: a -> Number
  startDate :: a -> JulianTime
  endDate :: a -> JulianTime
```

##### Instances
``` purescript
IsAsset ChkAct
IsAsset SavAct
IsAsset (AssetR a)
IsAsset (BasicAsset r)
IsAsset (BasicRetirement r)
IsAsset (RetirementR a)
IsAsset (SpecialRetirement r)
```

#### `ChkAct`

``` purescript
data ChkAct
  = Ck Number JulianTime JulianTime
```

##### Instances
``` purescript
IsAsset ChkAct
```

#### `SavAct`

``` purescript
data SavAct
  = Sv Number JulianTime JulianTime
```

##### Instances
``` purescript
IsAsset SavAct
```

#### `AssetF`

``` purescript
newtype AssetF a r
  = Asset (AssetType a r)
```

#### `AssetType`

``` purescript
type AssetType a r = { value :: a, currentBalance :: a -> Number, startDate :: a -> JulianTime, endDate :: a -> JulianTime | r }
```

#### `mkAssetF`

``` purescript
mkAssetF :: forall a. (IsAsset a) => a -> AssetF a ()
```

#### `AssetR`

``` purescript
newtype AssetR a
  = AR (ExistsR (AssetF a))
```

##### Instances
``` purescript
IsAsset (AssetR a)
```

#### `runAR`

``` purescript
runAR :: forall a. AssetR a -> ExistsR (AssetF a)
```

#### `Asset`

``` purescript
type Asset = Exists AssetR
```

#### `mkAsset`

``` purescript
mkAsset :: forall a. (IsAsset a) => a -> Asset
```

#### `assets`

``` purescript
assets :: Array Asset
```

#### `balances`

``` purescript
balances :: Array Asset -> String
```

#### `startDates`

``` purescript
startDates :: Array Asset -> String
```

#### `Test`

``` purescript
data Test a r
  = Test { a :: a, b :: String, c :: Boolean | r }
```

#### `testA`

``` purescript
testA :: Test String (d :: Number)
```

#### `testB`

``` purescript
testB :: Test String (e :: Int)
```

#### `StrRec`

``` purescript
type StrRec = ExistsR (Test String)
```

#### `mkStrRec`

``` purescript
mkStrRec :: forall r. Test String r -> StrRec
```

#### `testes`

``` purescript
testes :: Array StrRec
```

#### `stuff`

``` purescript
stuff :: forall r. Test String r -> String
```

#### `runStuff`

``` purescript
runStuff :: Array StrRec -> String
```

#### `TestR`

``` purescript
newtype TestR a
  = TR (ExistsR (Test a))
```

#### `runTR`

``` purescript
runTR :: forall a. TestR a -> ExistsR (Test a)
```

#### `TestS`

``` purescript
type TestS = Exists TestR
```

#### `mkTestS`

``` purescript
mkTestS :: forall a r. Test a r -> TestS
```

#### `testA1`

``` purescript
testA1 :: Test Int (d :: Number)
```

#### `testes1`

``` purescript
testes1 :: Array TestS
```

#### `stuff2`

``` purescript
stuff2 :: forall a r. Test a r -> String
```

#### `runTestes2`

``` purescript
runTestes2 :: Array TestS -> String
```

#### `BasicAsset`

``` purescript
newtype BasicAsset r
  = BasicAsset { balance :: Number, firstActivity :: JulianTime, lastActivity :: JulianTime | r }
```

##### Instances
``` purescript
IsAsset (BasicAsset r)
```

#### `myCheck`

``` purescript
myCheck :: BasicAsset ()
```

#### `mySaving`

``` purescript
mySaving :: BasicAsset ()
```

#### `BasicRetirement`

``` purescript
newtype BasicRetirement r
  = BasicRetirement { vestedBalance :: Number, totalBalance :: Number, firstActivity :: JulianTime, lastActivity :: JulianTime | r }
```

##### Instances
``` purescript
IsAsset (BasicRetirement r)
IsRetirement (BasicRetirement r)
```

#### `my401k`

``` purescript
my401k :: BasicRetirement ()
```

#### `myIRA`

``` purescript
myIRA :: BasicRetirement (bank :: String)
```

#### `myAssets`

``` purescript
myAssets :: Array Asset
```

#### `IsRetirement`

``` purescript
class (IsAsset a) <= IsRetirement a where
  vestedBalance :: a -> Number
  totalBalance :: a -> Number
```

##### Instances
``` purescript
IsRetirement (BasicRetirement r)
IsRetirement (RetirementR a)
IsRetirement (SpecialRetirement r)
```

#### `RetirementFields`

``` purescript
type RetirementFields a r = (vestedBalance :: a -> Number, totalBalance :: a -> Number | r)
```

#### `RetirementF`

``` purescript
newtype RetirementF a r
  = Retirement (AssetType a (RetirementFields a r))
```

#### `mkRetirementF`

``` purescript
mkRetirementF :: forall a. (IsRetirement a) => a -> RetirementF a ()
```

#### `RetirementR`

``` purescript
newtype RetirementR a
  = RR (ExistsR (RetirementF a))
```

##### Instances
``` purescript
IsAsset (RetirementR a)
IsRetirement (RetirementR a)
```

#### `runRR`

``` purescript
runRR :: forall a. RetirementR a -> ExistsR (RetirementF a)
```

#### `Retirement`

``` purescript
type Retirement = Exists RetirementR
```

#### `mkRetirement`

``` purescript
mkRetirement :: forall a. (IsRetirement a) => a -> Retirement
```

#### `SpecialRetirement`

``` purescript
newtype SpecialRetirement r
  = SR { spentAmount :: Number, originalAmount :: Number, firstActivityTime :: Days, lastActivityTime :: Days, totalAmount :: Number, unvestedAmount :: Number | r }
```

##### Instances
``` purescript
IsAsset (SpecialRetirement r)
IsRetirement (SpecialRetirement r)
```

#### `mySpecial`

``` purescript
mySpecial :: SpecialRetirement (poop :: Boolean)
```

#### `retirementAssets`

``` purescript
retirementAssets :: Array Retirement
```

#### `vestedBalances`

``` purescript
vestedBalances :: Array Retirement -> String
```

#### `totalBalances`

``` purescript
totalBalances :: Array Retirement -> String
```

#### `balancesR`

``` purescript
balancesR :: Array Retirement -> String
```

#### `startDatesR`

``` purescript
startDatesR :: Array Retirement -> String
```

#### `endDatesR`

``` purescript
endDatesR :: Array Retirement -> String
```


