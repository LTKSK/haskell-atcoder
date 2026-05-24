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
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Debug.Trace
import Numeric (showIntAtBase)

dbg :: (Show a) => a -> ()
dbg = (`traceShow` ())

solveNQueen n = go S.empty [1 .. n] []
  where
    -- 分解して考える。y,xに置く(yは1..n)、Setを更新する、次の列に進む、置き場がないなら1つ戻る
    -- queenをqueenを配置した時の、移動範囲を埋める。これからやりましょう
    -- putQueen :: Int -> Int -> S.Set (Int, Int)
    putQueen y x =
      S.fromList $
        (y, x)
          : ( filter (inRange ((1, 1), (n, n))) $
                concat
                  [ [ (y, x - d),
                      (y, x + d),
                      (y + d, x),
                      (y - d, x),
                      (y + d, x + d),
                      (y - d, x - d),
                      (y + d, x - d),
                      (y - d, x + d)
                    ]
                    | d <- [1 .. n]
                  ]
            )
    go s [] acc = [acc]
    -- x列目のどこに置けるか
    -- go s (c : cs) acc =
    go s (c : cs) acc =
      concat
        [ go (S.union s queenPos) cs (r : acc)
          | r <- [1 .. n],
            not $ S.member (r, c) s,
            let queenPos = putQueen r c
        ]

main :: IO ()
main = do
  -- 順列の全列挙
  let task1 = [1, 2, 5, 4, 3]
      ans1 = L.permutations task1
      task2 = 13
      task2_elems = [1, 3, 6, 33, 22, 11, 8]
      picks :: [a] -> [(a, [a])]
      picks [] = []
      picks (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]
      solve2 [] acc = acc == task2
      solve2 xs acc
        | acc == task2 = True
        | acc <= task2 = or [solve2 xs' (acc + x) | (x, xs') <- picks xs]
        | otherwise = False
      ans2 = solve2 task2_elems 0
  -- print ans1
  -- print ans2

  let n = 8
      ans = solveNQueen n
  print ans -- 解のリスト
  print $ length ans