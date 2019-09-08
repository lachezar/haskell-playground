{-# LANGUAGE MultiWayIf #-}

module Throwaway where 

import Data.List

g = \i -> i * 2
f = map g [1, 2, 3]

x = \x -> x + 5

daysOfActualWork :: [Bool] -> [Bool] -> [Bool]
daysOfActualWork officeOpenDays secretaryPresentDays = 
    r
    where (_, r) = daysOfActualWork' officeOpenDays secretaryPresentDays

daysOfActualWork' officeOpenDays secretaryPresentDays =
    foldl' 
        (\(spd, r) ood -> 
            (tail spd, r ++ [ood && head spd])) 
        (secretaryPresentDays, []) 
        officeOpenDays

daysOfActualWork'' :: [Bool] -> [Bool] -> (Int, [Bool])
daysOfActualWork'' officeOpenDays secretaryPresentDays =
    foldl'
        (\(i, finalList) isOfficeOpen ->
            (i + 1, finalList ++ [isOfficeOpen && (secretaryPresentDays !! i)]))
        (0, [])
        officeOpenDays

sum' :: [Int] -> Int
sum' list = foldl' (\r i -> r + i) 0 list

sumEven :: [Int] -> Int
sumEven list = 
    foldl' (\r i -> r + i) 0 even
    where 
        even = filter (\i -> i `mod` 2 == 0) list

doubleList :: [Int] -> [Int]
doubleList list =
    map (\i -> i * 2) list

containedCharacters :: [Char] -> [Char] -> Bool
containedCharacters chars string =
    foldl' (\r c -> (c `elem` string) && r) True chars

genEvenNumbers :: Int -> [Int]
genEvenNumbers n = 
    filter (\i -> i `mod` 2 == 0) [1..n]

genFirstNEvenNumbers :: Int -> [Int]
genFirstNEvenNumbers n = 
    map (\i -> i * 2) [1..n]

sumFirstNMultiples :: Int -> (Int, Int)
sumFirstNMultiples n = 
    foldl' (\(r, cn) i -> if
        | cn >= n -> (r, cn)
        | i `mod` 3 == 0 || i `mod` 5 == 0 -> (r + i, cn + 1)
        | otherwise -> (r, cn)
    ) (0, 0) [1..maxn]
    where 
        maxn = n * 5

take' :: [Int] -> Int -> [Int]
take' list n
    | list == [] = []
    | n <= 0 =  []
    | otherwise = (head list):(take' (tail list) (n-1))

sumFirstNMultiples' :: Int -> Int
sumFirstNMultiples' n = 
    sum' (take' (filter (\i -> i `mod` 3 == 0 || i `mod` 5 == 0) [1..5*n]) n)

fib :: Int -> Int
fib n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = (fib (n-1)) + (fib (n-2))

fibList :: Int -> [Int]
fibList n = map fib [1..n]

evenFibSum :: Int -> Int
evenFibSum n = sum' (filter (\i -> i `mod` 2 == 0) (fibList $ n*2))

prefixLineNumbers :: [String] -> [String]
prefixLineNumbers lines = map (\(i, s) -> show i ++ ": " ++ s) (zip [1..(length lines)] lines)

foldi :: (Int -> Bool) -> (Int -> Int) -> Int -> Int -> [Int] -> [Int]
foldi f g n cn cr
    | (length cr) >= n = cr
    | (f candidate) = (foldi f g n (cn+1) (cr ++ [candidate]))
    | otherwise = (foldi f g n (cn+1) cr)
    where 
        candidate = g (cn+1) 

fibListFilter :: (Int -> Bool) -> Int -> [Int]
fibListFilter f n = foldi f fib n 0 []
