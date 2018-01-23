-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

main :: IO ()
main = putStrLn . show . maximum $ [x*y | x <- [1..999], y <- [1..999], isPalindrome $ x*y]

isPalindrome :: Int -> Bool
isPalindrome n =
  let half = floor $ (fromIntegral . length $ show n) / 2
      s = splitAt half (show n)
      splits = if ((length $ fst s) /= (length $ snd s)) then (fst s, tail $ snd s) else s
  in fst splits == (reverse $ snd splits)
