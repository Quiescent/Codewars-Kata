module Difference where

difference :: Eq a => [a] -> [a] -> [a]
difference xs ys = filter (not . (`elem` ys)) xs
