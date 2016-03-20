module MonoidHomomorphisms (main) where

import Text.Printf
import Criterion.Main


add :: String -> String -> String
add = (++)


wordCount :: String -> Int
wordCount t =
  length $ words t


page :: String -> Int -> String
page word numberOfTimes =
  unwords $ replicate numberOfTimes word


pageHello :: Int -> String
pageHello = page "Hello"


main :: IO ()
main = 
  printf "The word count is %6i" $ wordCount $ page "Hello" 1000
