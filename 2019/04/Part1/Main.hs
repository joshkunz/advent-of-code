module Main where

import Data.List.Split;

digits :: Int -> [Int]
digits i =
    digitsR i []
    where digitsR i acc | i < 10 = i : acc
                        | otherwise = digitsR (i `div` 10) ((i `rem` 10) : acc) 

solve :: Int -> Int -> Int
solve from to =
    length $ filter id $ isValid <$> [from..to]
    where hasRepeating [] = False
          hasRepeating (_:[]) = False
          hasRepeating (i:j:rest) | i == j = True
                                  | otherwise = hasRepeating $ j : rest
          increasing [] = True
          increasing (_:[]) = True
          increasing (i:j:rest) | i > j = False 
                                | otherwise = increasing $ j : rest
          isValid = (\v -> min (hasRepeating v) (increasing v)) . digits

parse :: String -> Maybe (Int, Int)
parse s =
    toTuple $ read <$> splitOn "-" s
    where toTuple [a,b] = Just (a, b)
          toTuple _ = Nothing

main = interact $ show . (fmap $ uncurry solve) . parse



