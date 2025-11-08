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

solve' :: [Int] -> Int
solve' as = Seq.length $ foldl' step Seq.empty as
  where
    step tail x
      | Seq.null tail || x > Seq.index tail (Seq.length tail - 1) = tail Seq.|> x
      | otherwise =
          let pos = binarySearch tail x
           in Seq.update pos x tail

    binarySearch tail x = go 0 (Seq.length tail)
      where
        go l r
          | l >= r = l
          | Seq.index tail mid < x = go (mid + 1) r
          | otherwise = go l mid
          where
            mid = (l + r) `div` 2

solve :: [Int] -> Int
solve as = Seq.length $ foldl' step Seq.empty as
  where
    step tail x
      -- Seqが空またはtailの末尾の値より大きい
      | Seq.null tail || x > Seq.index tail (Seq.length tail - 1) =
          tail Seq.|> x
      | otherwise =
          let pos = binarySearch tail x
           in Seq.update pos x tail

    binarySearch tail x = go 0 (Seq.length tail)
      where
        go l r
          | l >= r = l
          -- ここではtailの中でx以上の値の最小のindex（lowebBound）を探している
          -- 条件が逆に見えるけど、x未満ならmidは探索範囲から消える
          | Seq.index tail mid < x = go (mid + 1) r
          | otherwise = go l mid
          where
            mid = (l + r) `div` 2

main :: IO ()
main = do
  [n] <- ints
  as <- ints
  print $ solve as
