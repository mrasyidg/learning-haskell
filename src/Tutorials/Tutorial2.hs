module Tutorials.Tutorial2(
    sumOfSquares
,   triangular
,   power
,   isPalindrome
) where
  
import Data.Char ( isAlpha, toLower )
import Data.List ()

-- 1. sumOfSquares [Nums]
sumOfSquares :: Num a => [a] -> a
sumOfSquares lst = sum $ map (^2) lst

-- 2. triangular x
triangular :: Integral a => a -> a
triangular x = x * (x + 1) `div` 2

-- 3. power x n
power:: Integer->Integer->Integer
power x n   | (n==1) = x
            | x < 0 || n < 0 = error "Error: inputs should only be positive integers."
            | even n = (power x ( div n 2))*(power x ( div n 2)) 
            | odd n  = x * (power x (n-1))

-- 4. isPalindrome str
isPalindrome :: String -> Bool
isPalindrome str = s1 == reverse(s1)
            where s1 = stringLowerLetters str

stringToLower :: String -> String
stringToLower str = map toLower str
                        
stringletters :: String -> String
stringletters str = filter isAlpha str
                        
stringLowerLetters :: String -> String
stringLowerLetters = stringToLower.stringletters