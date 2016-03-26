module FrequentWords (main) where

import qualified Data.Map as Map


mostFrequentWord :: String -> Map.Map String Int
mostFrequentWord page =
  foldl foldF Map.empty list
  where
    list = words page
    foldF :: Map.Map String Int -> String -> Map.Map String Int
    foldF map k = Map.insertWith (+) k 1 map


main :: IO ()
main = do
  putStrLn "Hello world"

