{-# LANGUAGE LambdaCase #-}
import Data.Char (digitToInt, isSpace)
import Data.List (sort, isSuffixOf, isPrefixOf, unfoldr)
import Data.Set(fromList, toList)
import Control.Monad (replicateM, msum)
import Control.Monad.RWS (MonadState(put))
import qualified Data.ByteString.Char8 as BS

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
intwo =
    ints >>= \case
        [x, y] -> return (x,y)
        _ -> error "引数足りない"

canMove :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
canMove (t1,x1,y1) (t2,x2,y2) =
    let dt = t2 - t1
        dist = abs (x1 - x2) + abs (y1 - y2)
    in even (dt+dist) && dist <= dt

main :: IO ()
main = do
    n <- readLn :: IO Int
    ps <- replicateM n $ do
        [t, x, y] <- ints
        return (t,x,y)
    let points = (0,0,0): ps
    let zipped = zip points (tail points)

    putStrLn $ if all (uncurry canMove) zipped then "Yes" else "No"
