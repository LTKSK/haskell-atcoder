{-# LANGUAGE LambdaCase #-}

import Control.Monad (forM_, msum, replicateM, when)
import Control.Monad.RWS (MonadState (put))
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unboxed (UArray)
import Data.Bits
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt, isSpace)
import Data.Ix
import Data.List (isPrefixOf, isSuffixOf, sort, unfoldr)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Vector.Unboxed qualified as VU
import Debug.Trace

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

-- fを満たす最小の値を探す
-- leftは探索範囲の左側で、答えの値を含まない。rightは右側で含む
binSearchMin :: (Integral t) => (t -> Bool) -> t -> t -> t
binSearchMin f !left !right
  | right - left == 1 = right
  | f mid = binSearchMin f left mid
  | otherwise = binSearchMin f mid right
  where
    mid = left + (right - left) `div` 2

binSearchMax :: (Integral t) => (t -> Bool) -> t -> t -> t
binSearchMax f !left !right
  | right - left == 1 = left
  -- binSearchMinと↑の条件は同じ。fがTrueだったらleftをあげていく
  | f mid = binSearchMax f mid right
  | otherwise = binSearchMax f left mid
  where
    mid = left + (right - left) `div` 2

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

-- solve :: [Int] -> Int -> Int -> UArray (Int, Int) Int
-- solve as n m = runSTUArray $ do
--   -- n枚目、いくつ無料になるかの整数表現、枚数。minで更新するので
--   -- 初期値はでっかく
--   dp <- newArray ((0, 0), (m, bit n - 1)) maxBound
--   writeArray dp (0, 0) 0
--   forM_ (zip [0 ..] as) $ \(i, a) -> do
--     forM_ [0 .. bit n - 1] $ \s -> do
--       cur <- readArray dp (i, s)
--       when (cur /= maxBound) $ do
--         next1 <- readArray dp (i + 1, s)
--         writeArray dp (i + 1, s) (min cur next1)
--         -- 更新
--         let st = s .|. a
--         next2 <- readArray dp (i + 1, st)
--         writeArray dp (i + 1, st) (min (cur + 1) next2)
--   return dp

solve :: [Int] -> Int -> Int -> Int
solve as n m = dp ! (m, bit n - 1)
  where
    dp = runSTUArray $ do
      -- (使ったクーポン枚数, 無料になる商品の集合) = 最小枚数
      arr <- newArray ((0, 0), (m, bit n - 1)) maxBound

      -- 初期状態: 0枚使って、何も買えない = 0枚
      writeArray arr (0, 0) 0

      forM_ (zip [0 ..] as) $ \(i, a) -> do
        forM_ [0 .. bit n - 1] $ \s -> do
          cur <- readArray arr (i, s)
          when (cur /= maxBound) $ do
            -- クーポン i を使わない場合
            next1 <- readArray arr (i + 1, s)
            writeArray arr (i + 1, s) (min cur next1)

            -- クーポン i を使う場合
            let st = s .|. a -- 集合の和
            next2 <- readArray arr (i + 1, st)
            writeArray arr (i + 1, st) (min (cur + 1) next2)

      return arr

main :: IO ()
main = do
  [n, m] <- ints
  as <- replicateM m $ do
    foldl (\acc b -> acc * 2 + b) 0 <$> ints
  -- 漸化式を考える
  -- 初期状態dp[0]は枚選んでいるので、S={}、答えは無し（-1）
  -- 1枚選んだ状態は,
  -- dp[1][S] = dp[0][S]
  -- dp[1][S&T] = min dp[1][S&T] dp[0][S] + 1

  let res = solve as n m
  print $ if res == maxBound then -1 else res
