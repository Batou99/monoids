module MonoidalValidation where

import qualified Data.List as List


data ValidationResult 
  = Success
  | Error [String]


validateBadWord :: String -> String -> ValidationResult
validateBadWord word text =
  if word `List.isInfixOf` text 
     then Success
     else Error ["String contains a bad word: " ++ word]


validateLength maxLength text =
  if length text <= maxLength
     then Success
     else Error ["String is too long"]


instance Monoid ValidationResult where
  mempty = Success
  mappend a Success = a
  mappend Success a = a
  mappend (Error x) (Error y) = Error (x ++ y)


instance Show ValidationResult where
  show Success = "Ok"
  show (Error msgs) = List.intercalate ", " msgs


main :: IO ()
main = do
  let phrase = "cobol has native support for monads"
      validations = [
        validateLength 10,
        validateBadWord "Monad",
        validateBadWord "Cobol"
        ]
  print $ foldMap (\x -> x phrase) validations
