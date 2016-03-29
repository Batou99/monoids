module NonAssociativeOps where

import Text.Printf
import Data.Monoid


data Substract = Substract Int deriving (Show)


instance Monoid Substract where
  mempty = Substract 0
  mappend (Substract a) (Substract b) = Substract (a + b)


applyTo :: Int -> Substract -> Int
applyTo num (Substract substract) = num - substract


toInt :: Substract -> Int
toInt (Substract num) = -num


main :: IO ()
main = do
  let nums = [1 :: Int, 2, 3]
  -- ((0 - 1) - 2) - 3
  printf "Folding from the left: %i\n" $ foldl (-) 0 nums
  -- 1 - (2 - (3 - 0))
  printf "Folding from the right: %i\n" $ foldr (-) 0 nums
  putStrLn "Now checking Substract typeclass"
  printf "Folding from the left: %i\n" $ toInt $ foldl mappend mempty $ map Substract nums
  printf "Folding from the right: %i\n" $ toInt $ foldr mappend mempty $ map Substract nums
  printf "Folding with foldMap: %i\n" $ toInt $ foldMap Substract nums






