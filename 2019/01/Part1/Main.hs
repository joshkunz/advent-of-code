module Main where
import System.IO;

fuelRequired :: (Integral a) => a -> a
fuelRequired n = (n `div` 3) - 2        -- Integer devision so no need to round

parse :: String -> [Int]
parse = map (read :: (String -> Int)) . lines

solve :: [Int] -> Int
solve = sum . map fuelRequired  

main = interact $ show . solve . parse
