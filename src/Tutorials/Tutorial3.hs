module Tutorials.Tutorial3(
    myCurry
,   myUncurry
,   fib
,   fibonacci
,   powerWithAdd
,   sumEven
,   invest
) where

-- 1. myCurry to myUncurry (uncurrying)
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f = \x y -> f (x, y)
myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f = \(x,y) -> f x y

-- 2. fibonacci Int
fib::Int->Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibonacci :: Int -> [Int]
fibonacci n = map fib [0..n] -- apply fib to all list items

-- 3. powerWithAdd with additions only
powerWithAdd :: Int -> Int -> Int
powerWithAdd x 0 = 1
powerWithAdd 0 n = 0
powerWithAdd x 1 = x
powerWithAdd x n  = let y = powerWithAdd x (n-1)
                        sum = 0
                    in iterate (\sum -> sum + x) 0 !! y

-- 4. sumEven [Int]
sumEven :: [Integer] -> Integer
sumEven = sum . filter even

-- 5. invest capital, interest rate, months
monthly :: Double -> Double -> Double
monthly a b = (a*(1 + b/100)) 

loop :: Double -> Double -> Double -> Double
loop a b 0 = a
loop a b c = monthly (loop a b (c-1)) b+a

invest :: Double -> Double -> Double -> Double
invest a b c = loop a b c - a