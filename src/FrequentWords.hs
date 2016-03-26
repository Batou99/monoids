module FrequentWords (main) where

import Text.Printf
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Monoid
import MonoidHomomorphisms (page)


mostFrequentWord :: String -> String
mostFrequentWord page =
  fst $ head sortedList
  where
    wordsmap = wordFrequencies page
    wordslist = Map.toList wordsmap
    sortedList = List.sortBy (\(_,v1) (_,v2) -> compare v2 v1) wordslist 


wordFrequencies :: String -> Map.Map String Int
wordFrequencies page =
  foldl foldF Map.empty list
  where
    list = words page
    foldF :: Map.Map String Int -> String -> Map.Map String Int
    foldF map k = Map.insertWith (+) k 1 map


page1 = page "hello world" 1000
page2 = page "goodbye world" 1000
page3 = page "foobar" 1000
document = [page1, page2, page3]


joinThenCount :: [String] -> String
joinThenCount = mostFrequentWord . mconcat 


mapThenAddCounts :: [String] -> String
mapThenAddCounts = foldMap mostFrequentWord


main :: IO ()
main = do
  printf "Concat pages then check freq: %s\n" $ joinThenCount document
  printf "Check freqs then concat: %s\n" $ mapThenAddCounts document

