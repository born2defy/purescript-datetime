## Module Data.HLists.ExistsR

#### `ExistsR`

``` purescript
data ExistsR :: (# * -> *) -> *
```

#### `mkExistsR`

``` purescript
mkExistsR :: forall f a. f a -> ExistsR f
```

#### `runExistsR`

``` purescript
runExistsR :: forall f r. (forall a. f a -> r) -> ExistsR f -> r
```


