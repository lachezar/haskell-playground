module Pangram where

import Data.Char (toLower)

charPresent :: Char -> String -> Bool
charPresent n str
    | str == "" = False
    | otherwise = n == (toLower $ head str) || charPresent n (tail str)

isPangram :: String -> String -> Bool
isPangram rc str
    | rc == "" = True
    | otherwise = (charPresent (head rc) str) && (isPangram (tail rc) str)

