
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

main :: IO ()
main = do
    [h,w] <- ints
    g <- getMatInt h w
    -- 行、列それぞれで集計
    let rowsums = listArray @UArray (0, h-1) [sum [g ! (y,x) | x <-[0..w-1]] | y <-[0..h-1]]
    let colsums = listArray @UArray (0, w-1) [sum [g ! (y,x) | y <-[0..h-1]] | x <-[0..w-1]]

    forM_ [0..h-1] $ \y -> do
        -- gの注目している座標の値が2回計上されているので引いておく必要あり
        let row = [rowsums ! y + colsums ! x - g ! (y,x) | x <-[0..w-1]]
        putStrLn $ unwords (map show row)
