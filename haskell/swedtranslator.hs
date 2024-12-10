module Swedtranslator (main)
where

import Data.Text (pack, unpack, replace, Text)
import Data.Char (toUpper, toLower, isUpper, isLower)
import Observation (original)
import SwedishDictionary (translations)


-- Function to adjust the case of the translated word
adjustCase :: String -> String -> String
adjustCase originalWord translatedWord
 | all isUpper originalWord = map toUpper translatedWord
 | all isLower originalWord = map toLower translatedWord
 | isUpper (head originalWord) =
   toUpper (head translatedWord) : tail translatedWord
 | otherwise = translatedWord

-- Function to perform case-insensitive translation
translate :: String -> String -> Text -> Text
translate old new text = 
   let lowerOld = map toLower old
       lowerText = unpack text
   in foldr (\word acc -> if map toLower word == lowerOld 
      then replace (pack word) (pack (adjustCase word new)) acc
      else acc) text (words lowerText)

-- Function to apply all translations
translateAll :: Text -> Text
translateAll text = foldr (\(old, new) acc
 -> translate old new acc) text translations

lastWord :: Text
lastWord = translateAll (pack original)

main :: IO ()
main = do
   putStrLn original
   putStrLn "_________________\f"
   putStrLn (unpack lastWord)
  
{- Comments by AI
Key Changes:
Initial Accumulator: The foldr function now takes text as the initial 
accumulator. This means that it will start with the original text and apply 
each translation in the translations list to it.
Function Signature: The lambda function inside foldr remains the same, but now 
it correctly applies the translations to the initial text.
With these changes, the code should compile and run without type issues, 
applying all translations to the original text as intended.

### Explanation of Changes:

1. **`adjustCase` Function**: This function checks the case of the 
`originalWord` and adjusts the `translatedWord` accordingly:
   - If the original word is all uppercase, it converts the translated word to
     uppercase.
   - If the original word is all lowercase, it converts the translated word to
     lowercase.
   - If the original word starts with an uppercase letter, it capitalizes the
    first letter of the translated word while keeping the rest unchanged.
   - Otherwise, it returns the translated word as is.

2. ** Modified `translate` Function**: The `translate` function now uses 
 `adjustCase` to ensure that the translated word maintains the case of the 
  original word.

With these changes, the code will now handle case insensitivity while 
preserving the original casing in the translated text.

Explanation of Changes:
Case-Insensitive Search: The translate function now performs a case-insensitive
search by converting both the old word and the words in the original text to
lowercase. It checks if the lowercase version of the word in the text matches
the lowercase version of the old word.
Preserving Original Casing: When a match is found, the adjustCase function is
used to ensure that the translated word retains the casing of the original
word.

Note:
This implementation assumes that the words in the original text are separated
by spaces. If your text contains punctuation or other delimiters, you may need
to adjust the logic to handle those cases appropriately.
With these changes, the code should now translate words regardless of their
casing in the original text while preserving the original casing in the final
output.
-}
