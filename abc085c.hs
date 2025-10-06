import Data.Char (digitToInt)
import Data.List (sort)
import Data.Set(fromList, toList)
import Control.Monad (replicateM)
import Control.Monad.RWS (MonadState(put))

ints :: IO [Int]
ints = map read . words <$> getLine
getints :: Int -> IO [Int]
getints n = replicateM n readLn :: IO [Int]


main :: IO ()
main = do
    [n, y] <- ints
    let conb = [(a,b,c) | a <- [0..n], b <- [0..n-a], let c = n - a - b, 10000*a + 5000*b + 1000*c == y]
    let res = case conb of
                [] -> "-1 -1 -1"
                _  -> let (a,b,c) = head conb in show a ++ " " ++ show b ++ " " ++ show c
    putStrLn res
