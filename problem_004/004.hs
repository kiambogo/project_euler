-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

main :: IO ()
main = putStrLn . show . maximum $ [x*y | x <- [1..999], y <- [1..999], isPalindrome $ x*y]

isPalindrome :: Int -> Bool
isPalindrome n = show n == (reverse $ show n)
