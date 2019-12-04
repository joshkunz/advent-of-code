module Main where

import qualified Data.Set as Set;
import Data.List.NonEmpty (nonEmpty);
import Data.List.Split;
import Data.Semigroup;
import Control.Monad;

data Movement = MRight | MLeft | MDown | MUp deriving (Eq,Show)
type Wire = [Movement]
data Point = Point Int Int deriving (Eq,Ord,Show)

wirePath :: Point -> Wire -> [Point]
wirePath center w =
    scanl step center w
    where step (Point x y) MRight = Point (x + 1) y
          step (Point x y) MLeft  = Point (x - 1) y
          step (Point x y) MDown  = Point x (y - 1) 
          step (Point x y) MUp    = Point x (y + 1)

parseMovement :: String -> [Movement]
parseMovement s =
    take (n s) . repeat . move $ s
    where move ('R':_) = MRight
          move ('L':_) = MLeft
          move ('D':_) = MDown
          move ('U':_) = MUp
          move _ = error "invalid movement" 
          n (_:count) = read count

parse :: String -> Maybe (Wire, Wire)
parse s =
    asWires $ parseWire <$> (take 2 . lines) s
    where asWires [a,b] = Just (a, b)
          asWires _ = Nothing
          parseWire w = mconcat $ parseMovement <$> splitOn "," w

solve :: Point -> (Wire, Wire) -> Maybe Int
solve center (w1, w2) = 
    getMin <$> sconcat <$> fmap Min <$> (fmap $ manhattan center) <$> intercepts 
    where pointSet = Set.fromList . wirePath center
          intPoints = Set.intersection (pointSet w1) (pointSet w2) Set.\\ Set.singleton center
          intercepts = (nonEmpty . Set.toList) intPoints
          manhattan (Point x1 y1) (Point x2 y2) = (abs (x1 - x2)) + (abs (y1 - y2))

origin = Point 0 0
main = interact $ show . (solve origin <=< parse)
