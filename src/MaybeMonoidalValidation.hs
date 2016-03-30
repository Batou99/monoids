module MaybeMonoidalValidation where

import Data.Monoid
import qualified Data.List as List


type Errors = [String]

validateBadWord :: String -> String -> Maybe Errors
validateBadWord word text =
  if word `List.isInfixOf` text 
     then Nothing
     else Just ["String contains a bad word: " ++ word]


validateLength :: Int -> String -> Maybe Errors
validateLength maxLength text =
  if length text <= maxLength
     then Nothing
     else Just ["String is too long"]


extractOrDefault :: Maybe Errors -> String -> String
extractOrDefault (Just x) _ = List.intercalate ", " x
extractOrDefault Nothing def = def


main :: IO ()
main = do
  let phrase = "cobol has native support for monads"
      validations = [
        validateLength 10,
        validateBadWord "Monad",
        validateBadWord "Cobol"
        ]
      values = foldMap (\x -> x phrase) validations
      
  print $ extractOrDefault values "All Ok"
