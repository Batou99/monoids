module Identity where

import Data.Monoid

data PositiveNumberOrIdentity
  = Positive Int
  | Zero


instance Monoid PositiveNumberOrIdentity where
  mempty = Zero
  mappend Zero i = i
  mappend i Zero = i
  mappend (Positive i1) (Positive i2) = Positive (i1 + i2)

-- ABSTRACT THE PATTERN

data Monoid a => NormalOrIdentity a
  = Normal a
  | Empty


instance Monoid a => Monoid (NormalOrIdentity a) where
  mempty = Empty
  mappend Empty x = x
  mappend x Empty = x
  mappend (Normal x) (Normal y) = Normal (x <> y)


main :: IO()
main = undefined

