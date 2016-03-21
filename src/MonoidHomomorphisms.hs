module MonoidHomomorphisms (main) where

import Text.Printf
import Criterion.Main
import Data.Monoid
import Data.Foldable


-- MONOID HOMOMORPHISM
-- Monoid [a] -> Monoid Sum Int
wordCount :: String -> Sum Int
wordCount t =
  Sum $ length $ words t


page :: String -> Int -> String
page word numberOfTimes =
  unwords $ replicate numberOfTimes word


pageHello :: Int -> String
pageHello = page "Hello"


document :: [String]
document = replicate 100 $ pageHello 1000


mapThenAddCounts :: [String] -> Int
mapThenAddCounts pages = 
  total
  where
    Sum total = foldMap wordCount pages
    -- Also 
    -- Sum total = mconcat $ map wordCount pages


joinThenCount :: [String] -> Int
joinThenCount pages =
  total
  where
    Sum total = wordCount $ mconcat pages


main :: IO ()
main = do
  printf "The word count is %6i\n" $ wordCount $ page "Hello" 1000
  defaultMain [
    bgroup "wordcount" [ bench "single page" $ nf wordCount $ page "Hello" 1000 
                       , bench "map then count" $ whnf mapThenAddCounts document
                       , bench "join then count" $ whnf joinThenCount document
                       ]
              ]
