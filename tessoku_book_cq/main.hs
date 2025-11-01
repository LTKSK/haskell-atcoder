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

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

safeIndexOp :: [(Bool, [Int])] -> Int -> (Bool, [Int])
safeIndexOp arr a
  | a >= 0 = arr !! a
  | otherwise = (False, [])

(!!!) = safeIndexOp

-- dpと、経路復元のためにasのvalueとindexを受ける
-- ただこれだとTLEしてしまう。配列アクセスがO(j)かかるため
step :: [(Bool, [Int])] -> (Int, Int) -> [(Bool, [Int])]
step dp (ai, av) = do
  j <- [0 .. (length dp - 1)]
  let (c1, p1) = dp !! j
      (c2, p2) = dp !!! (j - av)
      can = c1 || c2
      path = if c2 then ai : p2 else p1
  return (can, path)

-- array版
-- // 演算子を使ってArrayの状態を遷移させていく
step' :: Array Int (Bool, [Int]) -> (Int, Int) -> Array Int (Bool, [Int])
-- //の演算は、indexに指定した値を置き換える
step' dp (ai, av) = dp // updates
  where
    (0, s) = bounds dp
    updates =
      [ (j, (True, ai : p2)) -- 更新対象のindexとその値
        | j <- [av .. s],
          let (c1, _) = dp ! j, -- jの値を取得
          not c1, -- ガード節
          let (c2, p2) = dp ! (j - av), -- c1がFalseならc2の値を見に行く
          c2 -- ここもガード節
      ]

main :: IO ()
main = do
  [n, s] <- ints
  as <- ints

  -- let dp0 = (True, []) : replicate s (False, [])
  --     dp = foldl step dp0 (zip [1 ..] as)
  --     (res, path) = last dp
  --     len = length path
  -- print $ if len > 0 then len else -1
  -- putStrLn $ unwords $ map show $ reverse path

  let dp0 = listArray (0, s) ((True, []) : replicate s (False, []))
      dp = foldl step' dp0 (zip [1 ..] as)
      (res, path) = dp ! s
      len = length path
  print $ if len > 0 then len else -1
  putStrLn $ unwords $ map show $ reverse path
