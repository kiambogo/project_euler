-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a2 + b2 = c2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc
main =
    putStrLn . show . tripletsProduct . head $
    filter
        (\(a,b,c) ->
              a + b + c == 1000)
        triplets

type Triplet = (Int, Int, Int)

triplets :: [Triplet]
triplets =
    [ (a, b, c)
    | c <- [1 ..]
    , b <- [1 .. c - 1]
    , a <- [1 .. b - 1]
    , c ^ 2 == a ^ 2 + b ^ 2 ]

tripletsProduct :: Triplet -> Int
tripletsProduct (a,b,c) = a * b * c