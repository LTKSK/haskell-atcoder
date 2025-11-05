{-# LANGUAGE LambdaCase #-}

import Control.Monad (forM_, msum, replicateM, when)
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

negInf :: Int
negInf = minBound `div` 2

solve :: Int -> UArray Int Int -> UArray Int Int -> UArray Int Int
solve n as bs = runSTUArray $ do
  -- 加算オーバーフロー対策。適当に大きな値
  dp <- newArray (1, n) negInf

  -- 初期値は地点1でスコア0
  writeArray dp 1 0
  forM_ [1 .. n - 1] $ \i -> do
    cur <- readArray dp i
    when (cur /= negInf) $ do
      let a = as ! i
      let b = bs ! i
      pa <- readArray dp a
      pb <- readArray dp b
      writeArray dp a (max pa (cur + 100))
      writeArray dp b (max pb (cur + 150))
  return dp

main :: IO ()
main = do
  [n] <- ints
  as <- ints
  bs <- ints
  let as' = listArray (1, n) as
  let bs' = listArray (1, n) bs
  print $ (solve n as' bs') ! n

-- updatesを使った更新はn^2らしくて、TLEだった。sampleは解けるが...
-- let abs = zip3 as bs [1 ..]
--     dp0 = listArray (1, n) (replicate n 0) :: UArray Int Int
--     res = foldl step dp0 abs
--       where
--         step dp (a, b, i) = dp // updates
--           where
--             cur = dp ! i
--             pa = dp ! a
--             pb = dp ! b
--             updates = [(a, max pa (cur + 100)), (b, max pb (cur + 150))]
-- print $ res ! n
