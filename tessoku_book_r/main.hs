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

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

intwo =
  ints >>= \case
    [x, y] -> return (x, y)
    _ -> error "引数足りない"

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
-- concatで多次元配列を1次元配列に
getMatInt h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h ints

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

binSearch :: (Int -> Bool) -> Int -> Int -> Int
binSearch f ok ng
  | abs (ok - ng) <= 1 = ok
  | otherwise =
      let mid = (ok + ng) `div` 2
       in if f mid
            then binSearch f mid ng -- 条件を満たすならmidをokに
            else binSearch f ok mid -- 逆はngをmidに

safeIndexOp :: [Bool] -> Int -> Bool
safeIndexOp arr a
  | a >= 0 = arr !! a
  | otherwise = False

(!!!) = safeIndexOp

step :: [Bool] -> Int -> [Bool]
step dpI cardVal = do
  j <- [0 .. (length dpI - 1)]
  return $ (dpI !!! j) || (dpI !!! (j - cardVal))

main :: IO ()
main = do
  [n, s] <- ints
  as <- ints
  let dp0 = True : replicate s False
      dp = foldl step dp0 as
   in printYn $ last dp
