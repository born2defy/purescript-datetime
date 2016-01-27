module Data.Date.Utilities.Clamped
  ( Clamped, clamp
  ) where

-- | Takes a value and sanitizes it
class Clamped a where
  clamp :: a -> a
