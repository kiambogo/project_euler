-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

main :: IO ()
main = putStrLn . show . maximum $ factors 600851475143

factors :: Integer -> [Integer]
factors n = [x | x <- map floor $ [1..sqrt $ fromInteger n], n `mod` x == 0, isPrime x]

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = null [x | x <- map floor $ [2..sqrt $ fromInteger n], n `mod` x == 0, n /= x]
