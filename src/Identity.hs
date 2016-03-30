module Identity where

import Data.Monoid
import Text.Printf

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

-- Abstraction is Maybe in disguise!

extractOrDefault :: Maybe (Sum Integer) -> Integer -> Integer
extractOrDefault (Just x) _ = getSum x
extractOrDefault Nothing def = def


main :: IO()
main = do
  let value = foldMap (Just . Sum) [1,2]
  printf "1+2: %i\n" $ extractOrDefault value (-1)
  printf "1+0: %i\n" $ extractOrDefault (Just (Sum 1) <> Nothing) 99
  

