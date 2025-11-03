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

shakutori :: UArray Int Int -> UArray Int Int -> Int -> Int -> Int -> Int
shakutori as bs a b lim = go 0 1 0
  where
    go l r res
      | r == lim = res
      | l >= r = res
      -- 条件を満たさない時はlを進める
      | (((as ! r) - (as ! l)) < a) || (((bs ! r) - (bs ! l)) >= b) = go (l + 1) r res
      | otherwise = go l (r + 1) (res + (r - l))

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

main :: IO ()
main = do
  -- しゃくとりっぽい
  -- l,rの範囲が条件を満たす限りrを進める
  -- しゃくとりじゃなくて累積和か？
  -- 範囲を広げた時に、aだったらそのまま、bが増えたら条件を満たさない可能性がある
  -- 条件を満たさなくなった時にどうすっかだよな
  -- 文字列は少なくともAの分の文字を含むはず
  -- と思っていたのだけど、累積和を使うようだ(尺取りでも出来はするらしい)
  [n, a, b] <- ints
  s <- getLine
  let as = listArray (0, n) $ scanl (+) 0 $ map (\c -> if c == 'a' then 1 else 0) s :: UArray Int Int
      bs = listArray (0, n) $ scanl (+) 0 $ map (\c -> if c == 'b' then 1 else 0) s :: UArray Int Int

      c =
        sum
          -- indexの差ではなくその範囲の個数なので+1。index3～index3まで、を考えると1個
          [ max 0 (jb - ja + 1)
            | i <- [0 .. n - 1],
              let na = as ! i,
              let nb = bs ! i,
              -- as[j] - as[i] > aを変形すると↓
              let ja = binSearchMin (\j -> (as ! j) >= na + a) i (n + 1),
              let jb = binSearchMax (\j -> (bs ! j) < nb + b) i (n + 1),
              ja /= n + 1
          ]
  -- A以上になる位置、alと、B以上になる位置を2分探索で求める。そのlとrの差が条件を満たす
  -- 走査はrを固定して行う
  -- c = shakutori as bs a b (n + 1)
  -- sum
  --   -- 条件を満たす最小のindexを求めたいので、okにはindexの最大値を入れる
  --   -- めぐる式を使う場合、okの値が本当にokかを確かめられないのでこういう場合は使いづらい
  --   [ let ar = binSearch (\i -> ((as ! i) - (as ! l)) >= a) n l
  --         br = binSearch (\i -> ((bs ! i) - (bs ! l)) >= b) n l
  --      in -- nをokの初期値に入れているが、nが条件を満たすとは限らないので、nのままだったら該当なしとする
  --         max 0 (br - ar)
  --     | l <- [0 .. n - 1]
  --   ]
  print c
