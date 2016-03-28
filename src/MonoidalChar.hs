module MonoidalChar where

import Data.Char
import Data.Foldable
import Text.Printf


type MChar = String


toMChar :: Char -> MChar
toMChar x = [x]


main :: IO ()
main = do
  putStrLn $ foldMap toMChar ['a','b']
  printf "Estos son los signos de puntuacion: %s" $ foldMap toMChar punctuactionSigns
  where
    punctuactionSigns = filter isPunctuation [' '..'z']
