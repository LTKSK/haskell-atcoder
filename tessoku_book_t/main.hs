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

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

lcsTable :: BS.ByteString -> BS.ByteString -> UArray (Int, Int) Int
lcsTable xs ys = runSTUArray $ do
  arr <- newArray ((0, 0), (n, m)) 0
  forM_ [0 .. n - 1] $ \i -> do
    forM_ [0 .. m - 1] $ \j -> do
      if xs `BS.index` i == ys `BS.index` j
        then do
          a <- readArray arr (i, j)
          -- 同じだったら右下に+1
          writeArray arr (i + 1, j + 1) $! a + 1
        else do
          -- 違う時は左か右のいずれか大きい方の値を採用
          a <- readArray arr (i + 1, j)
          b <- readArray arr (i, j + 1)
          writeArray arr (i + 1, j + 1) $! max a b
  return arr
  where
    n = BS.length xs
    m = BS.length ys

-- 遅延評価の影響で無限loop。こういう場合もあるんだなぁ
-- 更新途中の前の段階の値が必要なので、素直にST使うのがいいっぽい
-- lcsTable' :: BS.ByteString -> BS.ByteString -> UArray (Int, Int) Int
-- lcsTable' xs ys = dp
--   where
--     n = BS.length xs
--     m = BS.length ys

--     bounds = ((0, 0), (n, m))
--     dp :: UArray (Int, Int) Int
--     dp = listArray ((0, 0), (n, m)) [go ij | ij <- range bounds]

--     go (0, 0) = 0
--     go (0, _) = 0
--     go (_, 0) = 0
--     go (i, j)
--       | xs `BS.index` (i - 1) == ys `BS.index` (j - 1) = dp ! (i - 1, j - 1) + 1
--       | otherwise = max (dp ! (i - 1, j)) (dp ! (i, j - 1))

main :: IO ()
main = do
  s <- BS.getLine
  t <- BS.getLine
  let n = BS.length s
      m = BS.length t
      dp = lcsTable s t
  print $ dp ! (n, m)
