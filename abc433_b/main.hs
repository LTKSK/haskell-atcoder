-- !で正格評価を強制
{-# LANGUAGE BangPatterns #-}
-- doやcaseに渡す関数のカッコを省略できる
{-# LANGUAGE BlockArguments #-}
-- \case -> が書けるようになる
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -O2 -Wno-unused-top-binds -Wno-unused-imports -Wno-orphans #-}

import Control.Monad (foldM, foldM_, forM_, msum, replicateM, replicateM_, unless, when)
import Control.Monad.RWS (MonadState (put))
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt, isSpace)
import Data.Heap qualified as H
import Data.Int (Int64)
import Data.IntMap.Strict qualified as IM
import Data.Ix
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Debug.Trace

-- デバッグ用
dbg :: (Show a) => a -> ()
dbg = (`traceShow` ())

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

-- buildDoubling n next: n要素、next!iが要素iの次の要素
buildDoubling :: Int -> Array Int Int -> Int -> Array (Int, Int) Int
buildDoubling n next maxK = dp
  where
    dp = listArray ((0, 1), (maxK, n)) [step k i | k <- [0 .. maxK], i <- [1 .. n]]
    step 0 i = next ! i
    step k i =
      let mid = dp ! (k - 1, i)
       in dp ! (k - 1, mid)

-- k回移動後の位置を求める
queryDoubling :: Array (Int, Int) Int -> Int -> Int -> Int
queryDoubling dp start k = foldl move start [0 .. maxK]
  where
    ((_, _), (maxK, _)) = bounds dp
    move pos bit
      | testBit k bit = dp ! (bit, pos)
      | otherwise = pos

buildSegTree :: (VUM.Unbox a) => a -> Int -> IO (VUM.IOVector a)
buildSegTree e n = VUM.replicate (n * 2) e

-- 一点更新
updateSegTree ::
  (VUM.Unbox a) =>
  (a -> a -> a) -> -- 演算
  VUM.IOVector a -> -- 木
  Int -> -- n (要素数)
  Int -> -- i (0-indexed)
  a -> -- 新しい値
  IO ()
updateSegTree op vec n i x = do
  let !pos = n + i
  VUM.write vec pos x
  updateParent (pos `div` 2)
  where
    updateParent !p
      | p < 1 = return ()
      | otherwise = do
          l <- VUM.read vec (2 * p)
          r <- VUM.read vec (2 * p + 1)
          VUM.write vec p (op l r)
          updateParent (p `div` 2)

-- 区間クエリ [l, r)
querySegTree ::
  (VUM.Unbox a) =>
  (a -> a -> a) -> -- 演算
  a -> -- 単位元
  VUM.IOVector a -> -- 木
  Int -> -- n (要素数)
  Int -> -- l (0-indexed)
  Int -> -- r (0-indexed, 含まない)
  IO a
querySegTree op e vec n l r = go (l + n) (r + n) e
  where
    go !l' !r' !acc
      | l' >= r' = return acc
      | otherwise = do
          acc1 <-
            -- 奇数であれば右の子であるのでその値を採用する
            if odd l'
              then op acc <$> VUM.read vec l'
              -- そうでなければ親の値を使う。偶数の場合右に兄弟ノードがあるため、その値を含めないと正しい値が分からない
              else return acc
          acc2 <-
            if odd r'
              -- queryは半開区間[)なので-1でr'を含まないようにする
              then op acc1 <$> VUM.read vec (r' - 1)
              else return acc1
          -- lが奇数である場合は値を参照したので、親の兄弟に遷移したい（木で見ると右上の親）
          -- 子iの親はi/2+1。divは切り捨てなのでl'+1を2で割っても同じ計算。5`div`2+1==6`div`2
          -- rは偶奇に関わらず親に向かう。rは含まれない範囲
          go ((l' + 1) `div` 2) (r' `div` 2) acc2

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

main :: IO ()
main = do
  [n] <- ints
  as <- ints
  let as' = listArray @UArray (1, n) as
  forM_ (zip [1 ..] as) $ \(i, a) -> do
    let arr = [v | v <- reverse [1 .. i], as' ! v > a]
    putStrLn $ if null arr then show (-1) else show $ head arr
