module FrequentWords (main) where

import qualified Data.Map as Map
import qualified Data.List as List


mostFrequentWord :: String -> String
mostFrequentWord page =
  fst $ head sortedList
  where
    wordsmap = wordFrequencies page
    wordslist = Map.toList wordsmap
    sortedList = List.sortBy (\(_,v1) (_,v2) -> compare v1 v2) wordslist 


wordFrequencies :: String -> Map.Map String Int
wordFrequencies page =
  foldl foldF Map.empty list
  where
    list = words page
    foldF :: Map.Map String Int -> String -> Map.Map String Int
    foldF map k = Map.insertWith (+) k 1 map


main :: IO ()
main = do
  putStrLn "Hello world"

