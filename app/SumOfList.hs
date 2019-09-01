module SumOfList where

sumOfList :: Int -> [Int] -> Int
sumOfList total [] = total
sumOfList total list =
    sumOfList (total + head list) $ tail list

sumOfEven :: Int -> [Int] -> Int
sumOfEven total [] = total
sumOfEven total list
    | x `mod` 2 == 0 = sumOfEven (total + x) $ tail list
    | otherwise = sumOfEven total $ tail list
    where x = head list

