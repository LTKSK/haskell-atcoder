{-# LANGUAGE LambdaCase #-}

import Control.Monad (forM_, msum, replicateM)
import Control.Monad.RWS (MonadState (put))
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt, isSpace)
import Data.Ix
import Data.List (isPrefixOf, isSuffixOf, sort, unfoldr)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Text.Parsec (updateState)

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

intwo =
  ints >>= \case
    [x, y] -> return (x, y)
    _ -> error "引数足りない"

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
-- concatで多次元配列を1次元配列に
getMatInt h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h ints

binSearch :: (Int -> Bool) -> Int -> Int -> Int
binSearch f ok ng
  | abs (ok - ng) <= 1 = ok
  | otherwise =
      let mid = (ok + ng) `div` 2
       in if f mid
            then binSearch f mid ng -- 条件を満たすならmidをokに
            else binSearch f ok mid -- 逆はngをmidに

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

main :: IO ()
main = do
  [n, maxW] <- ints
  wvs <- replicateM n ints
  let dp0 = listArray (0, maxW) (replicate (maxW + 1) 0) :: UArray Int Int
      dp = foldl step dp0 wvs
        where
          step :: UArray Int Int -> [Int] -> UArray Int Int
          step dp [w, v] = dp // updates
            where
              updates =
                [ (j, v')
                  | j <- [0 .. maxW],
                    j >= w,
                    let v' = max (dp ! j) ((dp ! (j - w)) + v)
                ]
   in -- i品物の個数、wを重さとしたdp[i][w]=vを考える
      -- vは最大値を取ればよい。選んだ場合と選ばなかった場合の最大値を参照
      -- w_iを現在選ぶか判断するアイテムの重み、v[i]はその価値として、
      -- dp[i][w] = max dp[i-1][w] dp[i-1][w+w_i] + v[i]
      print $ maximum (elems dp)
