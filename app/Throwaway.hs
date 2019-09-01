module Throwaway where

import Data.Char (toUpper, ord)

checkNumber :: Int -> String
checkNumber y 
    | y `mod` 2 == 0 = "even"
    | otherwise = "odd"

doubleList :: [Int] -> [Int] -> [Int]
doubleList a [] = a
doubleList a b =
    doubleList (a ++ [x * 2]) $ tail b
    where x = head b 

toUppercase :: String -> String -> String
toUppercase a [] = a
toUppercase a b = 
    toUppercase (a ++ [toUpper x]) $ tail b
    where x = head b

allCharsPresent :: String -> String -> Bool
allCharsPresent "" b = True
allCharsPresent a b = (isCharPresent (head a) b) && (allCharsPresent (tail a) b)

isCharPresent :: Char -> String -> Bool
isCharPresent x "" = False
isCharPresent x str = head str == x || (isCharPresent x $ tail str)

evenList :: Int -> [Int] -> [Int]
evenList n list 
    | n `mod` 2 == 1 = evenList (n-1) list
    | n <= 0 = list
    | otherwise = evenList (n-2) $ n:list

evenList' :: Int -> [Int] -> [Int]
evenList' n list
    | n <= 0 = list
    | otherwise = evenList' (n-1) $ (n * 2):list

multipleSum :: Int -> Int -> Int
multipleSum n total 
    | n < 3 = total
    | n `mod` 3 == 0 || n `mod` 5 == 0 = multipleSum (n-1) (total + n)
    | otherwise = multipleSum (n-1) total

multipleSum' :: Int -> Int -> Int
multipleSum' n total
    | n <= 0 = (multipleSum (total-1) 0)
    | total < 3 = multipleSum' n 3
    | total `mod` 3 == 0 || total `mod` 5 == 0 = multipleSum' (n-1) (total + 1)
    | otherwise = multipleSum' n (total + 1)
    
square :: Int -> Int
square n = n^2

sumOfSquares :: Int -> Int -> Int
sumOfSquares rc total 
    | rc <= 0 = total
    | otherwise = sumOfSquares (rc-1) (total + square rc)
    
squareOfSum :: Int -> Int -> Int
squareOfSum rc _total = square (rc * (1 + rc) `div` 2) 

difference :: Int -> Int
difference n = (sumOfSquares n 0) - (squareOfSum n 0)

fibonacci :: Int -> Int
fibonacci n
    | n <= 1 = 1
    | otherwise = fibonacci (n-1) + fibonacci (n-2)

evenSum :: Int -> Int -> Int -> Int
evenSum mv n total
    | mv < fibn = total
    | fibn `mod` 2 == 0 = evenSum mv (n+1) (total + fibn)
    | otherwise = evenSum mv (n+1) total
    where fibn = fibonacci n

isValidIsbn :: String -> Bool
isValidIsbn str = isValidIsbnInternal str 10 0

isValidIsbnInternal :: String -> Int -> Int -> Bool
isValidIsbnInternal str cm total
    | str == "" && cm /= 0 = False
    | str == "" && cm == 0 = total `mod` 11 == 0
    | n < 0 || n > 9 = isValidIsbnInternal (tail str) cm total
    | otherwise = isValidIsbnInternal (tail str) (cm-1) (n * cm + total)
    where 
        n = ((ord $ head str) - (ord '0')) 

withoutSpaces :: String -> String -> String
withoutSpaces input output
    | input == "" = output
    | n == ' ' = withoutSpaces (tail input) output
    | otherwise = withoutSpaces (tail input) (output ++ [n])
    where n = head input

encodeMessage :: String -> String -> String -> String -> Int -> Int -> String
encodeMessage input r1 r2 r3 cr md
    | input == "" = r1 ++ r2 ++ r3
    | cr == 1 && md == -1 = encodeMessage rest (r1 ++ [n]) r2 r3 2 (-1)
    | cr == 2 && md == -1 = encodeMessage rest r1 (r2 ++ [n]) r3 3 (-1)
    | cr == 3 && md == -1 = encodeMessage rest r1 r2 (r3 ++ [n]) 2 1
    | cr == 2 && md == 1 = encodeMessage rest r1 (r2 ++ [n]) r3 1 1
    | cr == 1 && md == 1 = encodeMessage rest (r1 ++ [n]) r2 r3 2 (-1)
    where 
        n = head input
        rest = tail input
