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
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt, intToDigit, isSpace, ord)
import Data.Heap qualified as H
import Data.Int (Int64)
import Data.IntMap.Strict qualified as IM
import Data.Ix
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord
import Data.STRef
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Debug.Trace
import Numeric (showIntAtBase)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

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

-- 畳み込みDP関数 ref : https://zenn.dev/naoya_ito/articles/87a8a21d52c302
-- 時間的遷移時に、状態空間が前の状態を引き継ぐ (accum)
-- ex) accumDP @UArray f max minBound (0, wx) [(0, 0)] wvs
accumDP ::
  ( IArray a e,
    Ix v,
    Eq e,
    Show e,
    Show v,
    Show (a v e),
    Foldable t
  ) =>
  ((v, e) -> x -> [(v, e')]) -> -- 状態遷移関数。遷移先一覧を返す
  (e -> e' -> e) -> -- 緩和の2項演算
  e -> -- 初期値
  (v, v) -> -- 状態空間の下界と上界
  [(v, e')] -> -- 開始時点の状態
  t x -> -- 入力
  a v e -- ArrayまたはUArray
accumDP f op initial (l, u) v0s xs = do
  let dp = accumArray op initial (l, u) v0s
  L.foldl' transition dp xs
  where
    transition dp x =
      accum op dp $
        -- fでdpの各要素からの遷移先が求まるので、それを範囲チェックした後accumで更新
        concatMap (filter (inRange (bounds dp) . fst) . (`f` x)) (assocs dp)

main :: IO ()
main = do
  [n, maxW] <- ints
  -- wvs <- replicateM n ints
  -- let dp0 = listArray (0, maxW) (replicate (maxW + 1) 0) :: UArray Int Int
  --     dp = foldl step dp0 wvs
  --       where
  --         step :: UArray Int Int -> [Int] -> UArray Int Int
  --         step dp [w, v] = dp // updates
  --           where
  --             updates =
  --               [ (j, v')
  --                 | j <- [0 .. maxW],
  --                   j >= w,
  --                   let v' = max (dp ! j) ((dp ! (j - w)) + v)
  --               ]
  --  in -- i品物の個数、wを重さとしたdp[i][w]=vを考える
  --     -- vは最大値を取ればよい。選んだ場合と選ばなかった場合の最大値を参照
  --     -- w_iを現在選ぶか判断するアイテムの重み、v[i]はその価値として、
  --     -- dp[i][w] = max dp[i-1][w] dp[i-1][w+w_i] + v[i]
  --     print $ maximum (elems dp)

  wvs <- replicateM n $ do
    [w, v] <- ints
    return (w, v)
  let dp = accumDP @UArray f max minBound (0, maxW) [(0, 0)] wvs
        where
          f (w, v) (wi, vi)
            | v == minBound = []
            | otherwise = [(w + wi, v + vi)]
  print $ maximum (elems dp)
