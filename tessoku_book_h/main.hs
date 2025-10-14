
{-# LANGUAGE LambdaCase #-}
import Data.Char (digitToInt, isSpace)
import Data.List (sort, isSuffixOf, isPrefixOf, unfoldr)
import Data.Set(fromList, toList)
import Control.Monad (replicateM, msum, forM_)
import Control.Monad.RWS (MonadState(put))
import qualified Data.ByteString.Char8 as BS
import Data.Ix
import Data.Array.IArray
import Data.Array(Array)
import Data.Array.Unboxed(UArray)

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
intwo =
    ints >>= \case
        [x, y] -> return (x,y)
        _ -> error "引数足りない"

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
-- concatで多次元配列を1次元配列に
getMatInt h w = listArray ((0,0), (h-1, w-1)) . concat <$> replicateM h ints

csumtwo :: Int -> Int -> [[Int]] -> UArray (Int, Int) Int
csumtwo h w g = listArray((1,1), (h+1, w+1)) (concat scanned')
    where
        scanned = map (scanl (+) 0) g
        -- 0で埋められている行が必要なのでw+1
        scanned' = scanl (zipWith (+)) (replicate (w+1) 0) scanned

main :: IO ()
main = do
    [h,w] <- ints
    g <- replicateM h ints
    [q] <- ints
    abcd <- replicateM q ints

    let csum = csumtwo h w g
    forM_ abcd $ \[a,b,c,d] -> do
        print (csum ! (c+1,d+1) + csum ! (a,b) - csum ! (c+1, b) - csum ! (a,d+1))
