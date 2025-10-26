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
csumtwo h w g = listArray((1,1), (h+1, w+1)) (concat scanned')
    where
        scanned = map (scanl (+) 0) g
        -- 0で埋められている行が必要なのでw+1
        scanned' = scanl (zipWith (+)) (replicate (w+1) 0) scanned

main :: IO ()
main = do
    [n] <- ints
    xys <- replicateM n ints
    [q] <- ints
    abcds <- replicateM q ints
    let counts = M.fromListWith (+) [((y,x), 1) | [x,y] <- xys] :: M.Map (Int, Int) Int

    let maxX = 1500
        maxY = 1500

    -- [[Int]]に変換
    let g = [[M.findWithDefault 0 (j,i) counts | j <- [1..maxY]] | i <- [1..maxX]]

    let csum = csumtwo 1500 1500 g

    forM_ abcds $ \[a,b,c,d] -> do
        print (csum ! (c+1,d+1) + csum ! (a,b) - csum ! (c+1, b) - csum ! (a,d+1))

