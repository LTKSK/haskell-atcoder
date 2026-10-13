-- !で正格評価を強制
{-# LANGUAGE BangPatterns #-}
-- doやcaseに渡す関数のカッコを省略できる
{-# LANGUAGE BlockArguments #-}
-- \case -> が書けるようになる
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -O2 -Wno-unused-top-binds -Wno-unused-imports -Wno-orphans #-}

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.RWS (MonadState (put))
import Control.Monad.ST
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.IO
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.Bool
import Data.ByteString.Char8 qualified as BS
import Data.Char (chr, digitToInt, intToDigit, isSpace, ord)
import Data.Coerce
import Data.Containers.ListUtils
import Data.Foldable (toList)
import Data.Heap qualified as H
import Data.IORef
import Data.Int (Int64)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Ix
import Data.List qualified as L
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord
import Data.STRef
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Tuple
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Debug.Trace
import Numeric (showBin, showIntAtBase)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

integers :: IO [Integer]
integers = L.unfoldr (BS.readInteger . BS.dropWhile isSpace) <$> BS.getLine

chars :: IO [Char]
chars = filter (not . isSpace) . BS.unpack <$> BS.getLine

toBinStr :: Int -> String
toBinStr n = showIntAtBase 2 intToDigit n ""

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

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

accumArrayDP ::
  ( IArray a e,
    Ix ix,
    Eq e,
    Show e,
    Show ix,
    Show (a ix e),
    Foldable t
  ) =>
  ((ix, e) -> x -> [(ix, e')]) -> -- 状態を遷移させる関数
  (e -> e' -> e) -> -- relax
  e -> -- 初期値。遷移が来ない時の値
  (ix, ix) -> -- 状態空間の上界と下界
  [(ix, e')] -> -- 開始時点の状態
  t x -> -- 入力
  a ix e -- 出力はArrayまたはUArray
accumArrayDP next relax ini bnds v0s xs = do
  let dp = accumArray relax ini bnds v0s
   in L.foldl' transition dp xs
  where
    transition dp x =
      accumArray relax ini bnds $
        -- dpの各添え字と値に対してnextを実行。この際入力のxが第二引数になる
        -- なのでdpの現在の状態に対して(xを選ぶ、xを選ばない)とそれぞれ決めた時の遷移が行われる
        -- 範囲外に出ていかないように添え字をチェックしてfilterを掛ける
        concatMap (filter (inRange (bounds dp) . fst) . (`next` x)) (assocs dp)

data Activity = A | B | C deriving (Ord, Eq, Show, Ix)

main :: IO ()
main = do
  [n] <- ints
  abcs <- replicateM n ints
  let dp = accumArrayDP @UArray next relax ini bnds v0s abcs
      relax = max
      bnds = (A, C)
      ini = minBound
      v0s = [(A, 0), (B, 0), (C, 0)]
      next (activity, v) [a, b, c]
        | v == minBound = []
        | otherwise = case activity of
            A -> [(B, b + v), (C, c + v)]
            B -> [(A, a + v), (C, c + v)]
            C -> [(B, b + v), (A, a + v)]

  print $ maximum [dp ! A, dp ! B, dp ! C]
