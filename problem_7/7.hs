-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10001st prime number?

main :: IO ()
main = putStrLn . show $ primes !! 10000

primes :: [Integer]
primes = filter isPrime [1..]

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = null [x | x <- map floor $ [2..sqrt $ fromInteger n], n `mod` x == 0, n /= x]
