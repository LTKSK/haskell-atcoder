
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

cumtwo :: Int -> Int -> [[Int]] -> [[Int]]
cumtwo h w g = result
    where
        -- queryの情報から、+1,-1の入った配列を作る
        -- いもす法: 矩形の左上に+1, 右外と下外に-1, 右下外に+1
        updates = concat
            [ [((a,b), 1), ((c+1,b), -1), ((a,d+1), -1), ((c+1,d+1), 1)] | [a,b,c,d] <- g ] :: [((Int, Int), Int)]
        diff = accumArray @UArray (+) 0 ((1,1), (h+1,w+1)) updates
        -- 横方向の累積和（各行ごとに左から累積）
        scanned_h = [ scanl1 (+) [diff ! (y,x) | x <- [1..w+1]]
                    | y <- [1..h+1]]
        -- 縦方向の累積和（各列ごとに上から累積）
        scanned_v = scanl1 (zipWith (+)) scanned_h
        -- scanl により h+2 行になる（先頭に0行が追加）

        -- 必要な部分だけ抽出: (1,1) から (h,w) まで
        -- scanned_vは(h+2)行×(w+2)列で、0-indexedなので
        -- 行は 1..h、列は 1..w を取る
        result = [ take w row | row <- take h scanned_v ]

main :: IO ()
main = do
    [h,w,n] <- ints
    abcds <- replicateM n ints
    let res = cumtwo h w abcds

    forM_ res $ \row -> do
        putStrLn $ unwords $ map show row
