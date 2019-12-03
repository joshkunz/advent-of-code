module Main where
import System.IO;

fuelRequiredRec :: (Integral a) => a -> a -> a
-- Integer devision so no need to round
fuelRequiredRec rem total =
    let additionalFuel = ((rem `div` 3) - 2)
    in if additionalFuel <= 0
       then total
       else fuelRequiredRec additionalFuel (total + additionalFuel)

fuelRequired :: (Integral a) => a -> a
fuelRequired n = fuelRequiredRec n 0

parse :: String -> [Int]
parse = map (read :: (String -> Int)) . lines

solve :: [Int] -> Int
solve = sum . map fuelRequired  

main = interact $ show . solve . parse
