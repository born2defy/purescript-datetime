module Data.HLists.ExistsR where

import Prelude
foreign import data ExistsR :: (# * -> *) -> *
foreign import mkExistsR :: forall f a. f a -> ExistsR f
foreign import runExistsR :: forall f r. (forall a. f a -> r) -> ExistsR f -> r
