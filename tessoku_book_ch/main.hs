{-# LANGUAGE LambdaCase #-}
import Data.Char (digitToInt, isSpace)
import Data.List (sort, isSuffixOf, isPrefixOf, unfoldr)
import Control.Monad (replicateM, msum, forM_)
import Control.Monad.RWS (MonadState(put))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import qualified Data.Map.Strict as M
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

binSearch :: (Int -> Bool) -> Int -> Int -> Int
binSearch f ok ng
  | abs (ok - ng) <= 1 = ok
  | otherwise =
    let mid = (ok + ng) `div` 2
    in if f mid
        then binSearch f mid ng -- 条件を満たすならmidをokに
        else binSearch f ok mid -- 逆はngをmidに

csumtwo :: Int -> Int -> [[Int]] -> UArray (Int, Int) Int
csumtwo h w g = listArray((0,0), (h, w)) (concat scanned')
    where
        scanned = map (scanl (+) 0) g
        -- 0で埋められている行が必要なのでw+1
        scanned' = scanl (zipWith (+)) (replicate (w+1) 0) scanned

main :: IO ()
main = do
    [n] <- ints
    abcds <- replicateM n ints
    -- 入力ばらして,mapに詰めて、1,0,-1のいずれかの入った[[Int]]
    -- 2次元累積和だとc+1,bに-1じゃないか？と思うのだけど、座標がマスの位置じゃなくて点なので座標はこれでよい
    -- 半開区間[A, C)みたいなイメージ
    let updates = concat [[((a,b), 1), ((a,d), -1), ((c,b), -1), ((c,d), 1)] | [a,b,c,d] <- abcds] :: [((Int, Int), Int)]
        counts = M.fromListWith (+) updates
        -- 最大が1500なので
        maxX = 1500
        maxY = 1500
        g = [[M.findWithDefault 0 (i,j) counts | j <- [0..maxY]] | i <- [0..maxX]]
        csum = csumtwo maxX maxY g

    print $ length [(i,j) | i <- [0..maxX], j <- [0..maxY], csum ! (i,j) > 0]

