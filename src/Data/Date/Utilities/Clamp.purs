module Data.Date.Utilities.Clamped
  ( Clamped, clamp
  ) where

class Clamped a where
  clamp :: a -> a
