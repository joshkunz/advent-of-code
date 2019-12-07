module Main where

import Data.List
import Data.List.Split
import Debug.Trace

data Orbit = Orbit String String deriving (Show)
data OrbitTree = Leaf String | Node String [OrbitTree] deriving (Show)

insertT :: OrbitTree -> Orbit -> OrbitTree
insertT (Leaf p) (Orbit from to) | from == p = Node from [Leaf to]
insertT (Node p os) (Orbit from to) | from == p = Node p (Leaf to : os)
insertT (Node p os) o = Node p $ (flip insertT) o <$> os
-- if we can't find the subtree to insert, do nothing
insertT ot _ = ot

planets :: OrbitTree -> [String]
planets (Leaf a) = [a]
planets (Node p ot) = p : (foldl mappend mempty $ planets <$> ot)

orbitSize :: OrbitTree -> String -> Int
orbitSize ot p =
    osR 0 ot
    where osR v (Leaf mP) | mP == p = v
                          | otherwise = 0
          osR v (Node mP nt) | mP == p = v
                             | otherwise = maximum $ osR (v + 1) <$> nt

unitTree = Leaf "COM"

orbitOf :: Orbit -> String
orbitOf (Orbit a _) = a

hasOrbitOf :: OrbitTree -> Orbit -> Bool
hasOrbitOf ot o = any (== (orbitOf o)) (planets ot)

toTree :: [Orbit] -> OrbitTree
toTree ivs = 
    toTreeR unitTree ivs
    where toTreeR ot [] = ot
          toTreeR ot xs = 
            trace ("iter on size " ++ (show $ length xs)) $ toTreeR nextT $ filterWith not
            where nextT = foldl insertT ot $ filterWith id
                  filterWith cond = filter (cond . hasOrbitOf ot) xs

parse :: String -> Maybe OrbitTree 
parse s = fmap toTree . sequence $ pOrbit <$> lines s
          where toOrbit [a, b] = Just $ Orbit a b
                toOrbit _ = Nothing
                pOrbit l = toOrbit $ splitOn ")" l

calcOrbits :: OrbitTree -> [(String, Int)]
calcOrbits t = zip (planets t) (orbitSize t <$> planets t)

solve :: OrbitTree -> Int
solve = sum . fmap snd . calcOrbits 

main = interact $ show . fmap solve . parse 
