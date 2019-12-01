import System.IO;
import Control.Monad;

fuelRequiredRec :: (Integral a) => a -> a -> a
-- Integer devision so no need to round
fuelRequiredRec rem total =
    let additionalFuel = ((rem `div` 3) - 2)
    in if additionalFuel <= 0
       then total
       else fuelRequiredRec additionalFuel (total + additionalFuel)

fuelRequired :: (Integral a) => a -> a
fuelRequired n = fuelRequiredRec n 0

getAllLines :: IO [String]
getAllLines = liftM lines $ hGetContents stdin

getModules :: IO [Int]
getModules = getAllLines >>= return . map (read :: (String -> Int))

allFuel = (liftM $ fmap fuelRequired) getModules

main = (liftM sum $ allFuel) >>= print
