import Text.Regex
import Text.Printf

data Text = Text String


add :: Text -> Text -> Text
add (Text t1) (Text t2) =
  Text (t1 ++ t2)


wordCount :: Text -> Int
wordCount (Text t) =
  length $ splitRegex (mkRegex "\\s+") t


main = 
  printf "The word count is %5i" $ wordCount (Text "Hello world")
