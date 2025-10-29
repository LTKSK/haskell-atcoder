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

solve :: [Int] -> Int
solve hs'@(h1 : h2 : hs) = res
  where
    (res, _) = foldl step (abs (h1 - h2), 0) (zip3 hs' (tail hs') (tail $ tail hs'))
    step (costPrev1, costPrev2) (hPrev2, hPrev1, hPrevcur) =
      (newCost, costPrev1)
      where
        costFrom2 = costPrev2 + abs (hPrev2 - hPrevcur)
        costFrom1 = costPrev1 + abs (hPrev1 - hPrevcur)
        newCost = min costFrom2 costFrom1

main :: IO ()
main = do
  [n] <- ints
  hs <- ints
  let res = solve hs
  print res
