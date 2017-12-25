module FindShortest where

find_shortest :: String -> Integer
find_shortest = fromIntegral . minimum . map length . words
