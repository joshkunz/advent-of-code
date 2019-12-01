import System.IO;
import Control.Monad;

fuelRequired :: (Integral a) => a -> a
fuelRequired n = (n `div` 3) - 2        -- Integer devision so no need to round

getAllLines :: IO [String]
getAllLines = liftM lines $ hGetContents stdin

getModules :: IO [Int]
getModules = getAllLines >>= return . map (read :: (String -> Int))

allFuel = (liftM $ fmap fuelRequired) getModules

main = (liftM sum $ allFuel) >>= print
