module Tutorials.Tutorial4(
    lengthWithFold
,   sumOfSquaresWithMap
,   sumOfSquaresWithList
,   multipleOf5
,   total
,   reverse'
,   pythaTriple
,   primes
,   alpha
,   noUpperIndent
,   listComp
) where

-- 1. lengthWithFold a
lengthWithFold :: [a] -> Int
lengthWithFold a = foldl (+) 0 (map (\x -> 1) a)

-- 2. addUp ns
addUp :: (Num b, Ord b) => [b] -> [b]
addUp ns = map (+1) (filter(\a -> a > 0) ns)

-- 3. sumOfSqures
--  a. with Map
sumOfSquaresWithMap :: (Num b, Enum b) => b -> b
sumOfSquaresWithMap a = foldr (+) 0 (map(\a->a^2) [1..a])
--  b. with List Comprehension
sumOfSquaresWithList :: (Num b, Enum b) => b -> b
sumOfSquaresWithList b = foldr (+) 0 [ i^2 | i <-[1..b] ]

-- 4. multipleOf5
multipleOf5 :: [Int] -> Int
multipleOf5 x = foldl (+) 0 (map (\x -> 1) (filter (test) x))
    where test x = mod x 5 == 0

-- 5. total f
total :: (Int -> Int) -> (Int -> Int)
total f n = sum (map f [0..n])

-- 6. reverse with foldr
reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++[x])[]

-- 7. listComp [Integer]
listComp :: [Integer]
listComp = [ x+y | x<-[1..4], y<-[2..4], x > y ]

-- 8 phytaTriple [(Int, Int, Int)]
pythaTriple :: [(Int, Int, Int)]
pythaTriple = [(x,y,z) | z <- [1..], y <- [1..z], x <- [1..y], x*x + y*y == z*z]

-- 9 Sieve of Eratosthenes
primes :: [Integer]
primes =  sieve [2..]
          where
          sieve (p:xs) = p : sieve [x | x <- xs, rem x p > 0]

-- 10 noUpperIndent str
alpha :: [Char]
alpha = ['a'..'z']
noUpperIndent :: String -> [Char]
noUpperIndent = filter(\x-> x `elem` alpha)