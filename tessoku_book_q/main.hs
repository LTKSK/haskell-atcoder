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

binSearch :: (Int -> Bool) -> Int -> Int -> Int
binSearch f ok ng
  | abs (ok - ng) <= 1 = ok
  | otherwise =
      let mid = (ok + ng) `div` 2
       in if f mid
            then binSearch f mid ng -- 条件を満たすならmidをokに
            else binSearch f ok mid -- 逆はngをmidに

--     res
--   where
--     (_, _, res) = foldl step (a1, 0 :: Int, [1]) (zip (zip as bs) [2 ..])
--     step (p1, p2, res) ((a, b), i) = if c1 < c2 then (c1, p1, res ++ [i]) else (c2, p1, res ++ [i])
--       where
--         c1 = p1 + a
--         c2 = p2 + b

-- ref: https://zenn.dev/ppdx999/books/65e882010be891/viewer/d9132d
solve :: [Int] -> [Int] -> [Int]
solve (a2 : as) bs =
  let -- 遷移時に経路も保存する
      room1 = (0, [1])
      room2 = (a2, [2, 1])
      calcNext ((c1, p1), (c2, p2)) (i, a, b) = min (c1 + a, i : p1) (c2 + b, i : p2)
      step acm curr = (calcNext acm curr, fst acm)
      (_, revPath) = fst $ foldl step (room2, room1) (zip3 [3 ..] as bs)
   in reverse revPath

main :: IO ()
main = do
  [n] <- ints
  as <- ints
  bs <- ints

  let res = solve as bs
  print $ length res
  putStrLn $ unwords $ map show res
