module MonoidalChar where

import Data.Char

type MChar = String


toMChar :: Char -> String
toMChar x = [x]


add :: MChar -> MChar -> MChar
add = (++)


main :: IO ()
main = do
  let a = toMChar 'a'
      b = toMChar 'b'
  putStrLn $ add a b
