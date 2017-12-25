module Disemvowel where

import Data.Char

notVowel :: Char -> Bool
notVowel = flip all ['a', 'e', 'i', 'o', 'u'] . (/=) . toLower

disemvowel :: String -> String
disemvowel = filter notVowel
