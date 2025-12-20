-- !で正格評価を強制
{-# LANGUAGE BangPatterns #-}
-- doやcaseに渡す関数のカッコを省略できる
{-# LANGUAGE BlockArguments #-}
-- \case -> が書けるようになる
{-# LANGUAGE LambdaCase #-}
-- if | で複数のパターン書き下せるようになる
{-# LANGUAGE MultiWayIf #-}
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

binSearch' :: (Int -> Bool) -> Int -> Int -> Maybe Int
binSearch' f !ok !ng
  | abs (ok - ng) <= 1 = if f ok then Just ok else Nothing
  | f mid = binSearch' f mid ng -- midがok
  | otherwise = binSearch' f ok mid -- midがng
  where
    mid = (ok + ng) `div` 2

main :: IO ()
main = do
  let arr = listArray (1, 10) [1, 3, 5, 7, 9, 11, 13, 15, 17, 19] :: UArray Int Int

  putStrLn "=== テスト1: 7以上の最小インデックス ==="
  let result1 = binSearch' (\i -> arr ! i >= 7) 10 0
  print result1 -- Just 4 (値は7)
  case result1 of
    Just i -> putStrLn $ "arr[" ++ show i ++ "] = " ++ show (arr ! i)
    Nothing -> putStrLn "見つからなかった"

  putStrLn "\n=== テスト2: 100以上の最小インデックス（見つからない） ==="
  let result2 = binSearch' (\i -> arr ! i >= 100) 10 0
  print result2 -- Nothing
  putStrLn "\n=== テスト3: 0以上の最小インデックス（全て満たす） ==="
  let result3 = binSearch' (\i -> arr ! i >= 0) 10 0
  print result3 -- Just 1 (値は1)
  case result3 of
    Just i -> putStrLn $ "arr[" ++ show i ++ "] = " ++ show (arr ! i)
    Nothing -> putStrLn "見つからなかった"

  putStrLn "\n=== テスト4: 10以下の最大インデックス（逆向き探索） ==="
  let result4 = binSearch' (\i -> arr ! i <= 10) 1 11
  print result4 -- Just 5 (値は9)
  case result4 of
    Just i -> putStrLn $ "arr[" ++ show i ++ "] = " ++ show (arr ! i)
    Nothing -> putStrLn "見つからなかった"

  putStrLn "\n=== テスト5: リスト上での探索 ==="
  let list = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
  let result5 = binSearch' (\i -> list !! (i - 1) >= 10) 10 0
  print result5 -- Just 5 (値は10)
  case result5 of
    Just i -> putStrLn $ "list[" ++ show (i - 1) ++ "] = " ++ show (list !! (i - 1))
    Nothing -> putStrLn "見つからなかった"

  putStrLn "\n=== テスト6: 境界値テスト ==="
  let result6 = binSearch' (\i -> arr ! i >= 19) 10 0
  print result6 -- Just 10 (値は19)
  case result6 of
    Just i -> putStrLn $ "arr[" ++ show i ++ "] = " ++ show (arr ! i)
    Nothing -> putStrLn "見つからなかった"