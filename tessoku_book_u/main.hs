{-# LANGUAGE LambdaCase #-}

import Control.Monad (forM_, msum, replicateM)
import Control.Monad.RWS (MonadState (put))
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt, isSpace)
import Data.Ix
import Data.List (isPrefixOf, isSuffixOf, sort, unfoldr)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Vector.Unboxed qualified as VU
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

-- 参考: https://zenn.dev/ppdx999/books/65e882010be891/viewer/9a17ec
blockDp :: Array Int (Int, Int) -> Int -> Array (Int, Int) Int
blockDp blocks n = dp
  where
    bounds = ((1, 1), (n, n))

    p i = fst $ blocks ! i
    a i = snd $ blocks ! i
    -- dp の遷移は、ブロックがl~rまで残っている状態での最大スコアである
    dp = listArray bounds $ map step (range bounds)
    step (l, r)
      -- 何も取っていない時
      | l == 1 && r == n = 0
      -- 右側だけとった状態
      | l == 1 = rightScore
      -- 左側だけとった状態
      | r == n = leftScore
      -- それ以外はscoreが大きい方
      | otherwise = max leftScore rightScore
      where
        score i
          -- 取った個数の算出。piが残った区間の範囲に入っているなら取得できる
          -- l,rを1basedにしておくとここでの計算が楽っぽいな
          | l <= p i && p i <= r = a i
          | otherwise = 0
        leftScore = score (l - 1) + dp ! (l - 1, r)
        rightScore = score (r + 1) + dp ! (l, r + 1)

-- dp =
--   array
--     ((0, 0), (n - 1, n - 1))
--     [((l, r), go l r) | l <- [0 .. n - 1], r <- [0 .. n - 1]]

-- go l r
--   | l == 0 && r == n - 1 = 0
--   | l > r = 0
--   | otherwise =
--       let taken = l + (n - 1 - r)
--           nextOrder = taken + 1
--           (pl, al) = blocks ! l
--           (pr, ar) = blocks ! r
--           leftScore =
--             (if pl >= nextOrder then al else 0) + dp ! (l + 1, r)
--           -- 右端を取る
--           rightScore = (if pr >= nextOrder then ar else 0) + dp ! (l, r - 1)
--           result = max leftScore rightScore
--        in --  in max leftScore rightScore
--           trace
--             ( "go "
--                 ++ show (l, r)
--                 ++ " taken="
--                 ++ show taken
--                 ++ " next="
--                 ++ show nextOrder
--                 ++ " pl="
--                 ++ show pl
--                 ++ " pr="
--                 ++ show pr
--                 ++ " result="
--                 ++ show result
--            )
--             result

main :: IO ()
main = do
  [n] <- ints
  pas <- replicateM n ints
  let pas' = listArray (1, n) $ map (\[p, a] -> (p, a)) pas :: Array Int (Int, Int)
  let res = blockDp pas' n
  print $ maximum [res ! (i, i) | i <- [1 .. n]]
