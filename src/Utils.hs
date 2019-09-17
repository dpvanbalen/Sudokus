module Utils where

import Data.List
import Data.Maybe



update :: (a -> a) -> Int -> [a] -> [a]
update _ _ []     = []
update f 0 (x:xs) = f x : xs
update f n (x:xs) = x : update f (n-1) xs

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (_ : xs) = firstJust xs

