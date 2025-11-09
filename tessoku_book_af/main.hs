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
import Data.List (foldl', isPrefixOf, isSuffixOf, sort, unfoldr)
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Vector.Unboxed qualified as VU

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

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

solve :: Int -> Int -> Int -> Array Int Bool
solve n a b = dp
  where
    dp = listArray (0, n) (map step [0 .. n])
    step i
      -- 勝ちと負けを順に遷移していくイメージ
      | i >= a && not (dp ! (i - a)) = True
      | i >= b && not (dp ! (i - b)) = True
      | otherwise = False -- 残りの石がa個,b個いずれも足りない

main :: IO ()
main = do
  [n, a, b] <- ints
  let dp = solve n a b
  putStrLn $ if dp ! n then "First" else "Second"