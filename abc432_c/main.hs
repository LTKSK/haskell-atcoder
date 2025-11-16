-- !で正格評価を強制
{-# LANGUAGE BangPatterns #-}
-- doやcaseに渡す関数のカッコを省略できる
{-# LANGUAGE BlockArguments #-}
-- \case -> が書けるようになる
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -O2 -Wno-unused-top-binds -Wno-unused-imports -Wno-orphans #-}

import Control.Monad (forM_, msum, replicateM, when)
import Control.Monad.RWS (MonadState (put))
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt, isSpace)
import Data.Int (Int64)
import Data.IntMap.Strict qualified as IM
import Data.Ix
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Vector.Unboxed qualified as VU
import Debug.Trace

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- 数字が複数行入った時
intsN :: Int -> IO [Int]
intsN n = map head <$> replicateM n ints

intwo =
  ints >>= \case
    [x, y] -> return (x, y)
    _ -> error "引数足りない"

-- A 13 のような入力をparse
charInt :: IO (Char, Int)
charInt = do
  line <- BS.getLine
  let ws = BS.words line
  case ws of
    [c, n] -> case BS.readInt n of
      Just (num, _) -> return (BS.head c, num)
      Nothing -> error "数字のパースに失敗"
    _ -> error "フォーマットが違う"

intChar :: IO (Int, Char)
intChar = do
  line <- BS.getLine
  let ws = BS.words line
  case ws of
    [n, c] -> case BS.readInt n of
      Just (num, _) -> return (num, BS.head c)
      Nothing -> error "数字のパースに失敗"
    _ -> error "フォーマットが違う"

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
-- concatで多次元配列を1次元配列に
getMatInt h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h ints

getMatChar :: Int -> Int -> IO (UArray (Int, Int) Char)
-- concatで多次元配列を1次元配列に
getMatChar h w = listArray ((1, 1), (h, w)) . concat <$> replicateM h getLine

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

-- エラトステネスの篩
sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
  arr <- newArray (0, n) True
  writeArray arr 0 False
  writeArray arr 1 False
  -- Intそのままだと通らないのでfromIntegralを通す
  forM_ [2 .. floor $ sqrt $ fromIntegral n] $ \i -> do
    isPrime <- readArray arr i
    when isPrime $
      -- iの倍数を列挙する。haskellのlist記法は開始の値,次の値を記載できて、その差が増分になる
      -- iの倍数を列挙する。haskellのlist記法は開始の値,次の値を記載できて、その差が増分になる
      -- 例 [5, 8..50] -> [5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50]
      -- 例 [5, 8..50] -> [5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50]

      -- iの倍数を列挙する。haskellのlist記法は開始の値,次の値を記載できて、その差が増分になる
      -- 例 [5, 8..50] -> [5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50]
      forM_ [i * i, i * i + i .. n] $ \j ->
        writeArray arr j False
  return arr

-- 高速べき乗
powMod :: Int -> Int -> Int -> Int
powMod a b m
  | b == 0 = 1
  | even b = powMod ((a * a) `mod` m) (b `div` 2) m
  | otherwise = (a * powMod a (b - 1) m) `mod` m

-- もじゅーら計算
modulus :: Int64
modulus = 1_000_000_007

addMod, subMod, mulMod :: Int64 -> Int64 -> Int64
addMod x y = (x + y) `mod` modulus
subMod x y = (x - y) `mod` modulus
mulMod x y = (x * y) `mod` modulus

-- combination
comb :: Int -> Int -> Int
comb n m = product [n - m + 1 .. n] `div` product [1 .. m]

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

main :: IO ()
main = do
  [n, x, y] <- ints
  as <- ints
  -- 全探索
  -- aiにおいて、飴x,yを取得した時の個数と結果をtupleで保持する
  -- let s = [[(x', y', x * x' + y * y') | x' <- [0 .. a], let y' = x - x'] | a <- as]
  -- 計算が重すぎる...全探索じゃないっぽい

  -- ありえる最大値だから、mina*yの値に揃えるのが一番大きくなる、かも？
  -- maxa * x < mina *y
  -- let d = y - x
  --     maxa = maximum as
  --     mina = minimum as
  --     my = mina * y
  --     -- 全部のaが大きな飴を取ったと仮定して、最大との差分があるはず
  --     -- 例えばaが5,7で、yが5なら25,35。その差は10。これを、y-xで割って、その個数分だけ大きな飴から引く
  --     r =
  --       sum
  --         [c | a' <- as, let diffa = a' * y - my, let c | a' == mina = a' | diffa `mod` d /= 0 = minBound | otherwise = a' - diffa `div` d]
  -- print $ if r <= 0 then -1 else r

  let ma = y * minimum as
      mi = x * maximum as
      res =
        sum <$> sequence do
          a <- as
          return
            if mi <= ma && (ma - a * x) `mod` (y - x) == 0
              then Just $ (ma - a * x) `div` (y - x)
              else Nothing
  print $ fromMaybe (-1) res
