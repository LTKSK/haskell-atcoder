import Data.Char (digitToInt)
import Data.List (sort)
import Data.Set(fromList, toList)
import Control.Monad (replicateM)

ints :: IO [Int]
ints = map read . words <$> getLine
getints :: Int -> IO [Int]
getints n = replicateM n readLn :: IO [Int]


main :: IO ()
main = do
    [n] <- ints
    ds <- getints n
    let res = length $ toList $ fromList ds

    print res
