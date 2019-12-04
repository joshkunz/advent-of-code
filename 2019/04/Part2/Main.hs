module Main where

import Data.List.Split;
import Data.Function;
import Debug.Trace;

digits :: Int -> [Int]
digits i =
    digitsR i []
    where digitsR i acc | i < 10 = i : acc
                        | otherwise = digitsR (i `div` 10) ((i `rem` 10) : acc) 

solve :: Int -> Int -> Int
solve from to =
    length $ filter id $ isValid [hasRepeating, increasing] <$> [from..to]
    where hasRepeating [] = False
          hasRepeating [_] = False
          hasRepeating [i,j] | i == j = True
                             | otherwise = False
          hasRepeating (i:j:k:rest) | i == j && j /= k = True
                                    -- dropWhile to avoid cases like 123444
                                    -- guarded on i = j to avoid 357778
                                    | i == j = hasRepeating $ dropWhile ((==) j) rest
                                    | otherwise = hasRepeating $ j : k : rest
          increasing [] = True
          increasing (_:[]) = True
          increasing (i:j:rest) | i > j = False 
                                | otherwise = increasing $ j : rest
          isValid conds = foldl (&&) True . (\x -> (&) x <$> conds) . digits

parse :: String -> Maybe (Int, Int)
parse s =
    toTuple $ read <$> splitOn "-" s
    where toTuple [a,b] = Just (a, b)
          toTuple _ = Nothing

main = interact $ show . (fmap $ uncurry solve) . parse



