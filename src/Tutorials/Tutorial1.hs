module Tutorials.Tutorial1(
    square
,   circleArea
,   isTriangle
,   listSum
,   listSumArea
,   reverseList
,   quicksort1
) where

-- 1. circleArea r
square :: Num a => a -> a
square x = x * x 

circleArea :: Floating a => a -> a
circleArea r = 3.14 * square r 

-- 2. isTriangle a b c
isTriangle :: (Ord a, Num a) => a -> a -> a -> Bool
isTriangle a b c = a + b >= c && a + c >= b && b + c >= a 

-- 3. listSum [Int]
listSum :: [Int] -> Int
listSum [] = 0
listSum (x : xs) = x + listSum xs

-- 4. listSumArea [Double]
listSumArea :: [Double] -> Double
listSumArea [] = 0
listSumArea (x : xs) = x + listSumArea xs

-- 5. reverseList [a]
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- 6. Descending quicksort [Int]
quicksort1 :: Ord a => [a] -> [a]
quicksort1 [] = []
quicksort1 (x:xs) = quicksort1 small ++ (x : quicksort1 large)
  where small = [y | y <- xs, y >= x]
        large = [y | y <- xs, y < x]
