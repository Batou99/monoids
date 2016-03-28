module Validation where

import qualified Data.List as List


data ValidationResult
  = Success
  | Error String


validateBadWord :: String -> String -> ValidationResult
validateBadWord word text =
  if word `List.isInfixOf` text 
     then Success
     else Error $ "String contains a bad word: " ++ word

validateLength maxLength text =
  if length text <= maxLength
     then Success
     else Error "String is too long"


main :: IO ()
main = undefined
