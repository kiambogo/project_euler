-- -The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.
main = putStrLn . show . sum $ takeWhile (< 2000000) primes

primes :: [Integer]
primes = filter isPrime [1 ..]

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n =
    null
        [ x
        | x <- map floor $ [2 .. sqrt $ fromInteger n]
        , n `mod` x == 0
        , n /= x ]