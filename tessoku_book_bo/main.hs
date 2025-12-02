-- !で正格評価を強制
{-# LANGUAGE BangPatterns #-}
-- doやcaseに渡す関数のカッコを省略できる
{-# LANGUAGE BlockArguments #-}
-- \case -> が書けるようになる
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -O2 -Wno-unused-top-binds -Wno-unused-imports -Wno-orphans #-}

import Control.Monad
import Control.Monad.RWS (MonadState (put))
import Control.Monad.ST
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt, isSpace)
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
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Debug.Trace

-- デバッグ用
dbg :: (Show a) => a -> ()
dbg = (`traceShow` ())

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- 数字が複数行入った時
intsN :: Int -> IO [Int]
intsN n = map head <$> replicateM n ints

intwo =
  ints >>= \case
    [x, y] -> return (x, y)
    _ -> error "引数足りない"

-- A 13 のような入力をparse
charInt :: IO (Char, Int)
charInt = do
  line <- BS.getLine
  let ws = BS.words line
  case ws of
    [c, n] -> case BS.readInt n of
      Just (num, _) -> return (BS.head c, num)
      Nothing -> error "数字のパースに失敗"
    _ -> error "フォーマットが違う"

intChar :: IO (Int, Char)
intChar = do
  line <- BS.getLine
  let ws = BS.words line
  case ws of
    [n, c] -> case BS.readInt n of
      Just (num, _) -> return (num, BS.head c)
      Nothing -> error "数字のパースに失敗"
    _ -> error "フォーマットが違う"

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
-- concatで多次元配列を1次元配列に
getMatInt h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h ints

getMatChar :: Int -> Int -> IO (UArray (Int, Int) Char)
-- concatで多次元配列を1次元配列に
getMatChar h w = listArray ((1, 1), (h, w)) . concat <$> replicateM h getLine

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

-- エラトステネスの篩
sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
  arr <- newArray (0, n) True
  writeArray arr 0 False
  writeArray arr 1 False
  -- Intそのままだと通らないのでfromIntegralを通す
  forM_ [2 .. floor $ sqrt $ fromIntegral n] $ \i -> do
    isPrime <- readArray arr i
    when isPrime $
      -- iの倍数を列挙する。haskellのlist記法は開始の値,次の値を記載できて、その差が増分になる
      -- 例 [5, 8..50] -> [5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50]
      forM_ [i * i, i * i + i .. n] $ \j ->
        writeArray arr j False
  return arr

-- 高速べき乗
powMod :: Int -> Int -> Int -> Int
powMod a b m
  | b == 0 = 1
  | even b = powMod ((a * a) `mod` m) (b `div` 2) m
  | otherwise = (a * powMod a (b - 1) m) `mod` m

-- もじゅーら計算
modulus :: Int64
modulus = 1_000_000_007

addMod, subMod, mulMod :: Int64 -> Int64 -> Int64
addMod x y = (x + y) `mod` modulus
subMod x y = (x - y) `mod` modulus
mulMod x y = (x * y) `mod` modulus

-- combination
comb :: Int -> Int -> Int
comb n m = product [n - m + 1 .. n] `div` product [1 .. m]

-- buildDoubling n next: n要素、next!iが要素iの次の要素
buildDoubling :: Int -> Array Int Int -> Int -> Array (Int, Int) Int
buildDoubling n next maxK = dp
  where
    dp = listArray ((0, 1), (maxK, n)) [step k i | k <- [0 .. maxK], i <- [1 .. n]]
    step 0 i = next ! i
    step k i =
      let mid = dp ! (k - 1, i)
       in dp ! (k - 1, mid)

-- k回移動後の位置を求める
queryDoubling :: Array (Int, Int) Int -> Int -> Int -> Int
queryDoubling dp start k = foldl move start [0 .. maxK]
  where
    ((_, _), (maxK, _)) = bounds dp
    move pos bit
      | testBit k bit = dp ! (bit, pos)
      | otherwise = pos

csum2 :: UArray (Int, Int) Int -> UArray (Int, Int) Int
csum2 as = runSTUArray $ do
  -- 境界処理簡略化でboundsを1行1列追加で用意して初期化
  arr <- newArray ((h' - 1, w' - 1), (h, w)) 0

  forM_ (range $ bounds as) $ \(i, j) -> do
    -- 累積和の計算。一つ上と一つ左の値を足して、重複するi'j'を引く
    i'j <- readArray arr (i - 1, j)
    ij' <- readArray arr (i, j - 1)
    i'j' <- readArray arr (i - 1, j - 1)
    writeArray arr (i, j) $! as ! (i, j) + i'j + ij' - i'j'
  return arr
  where
    ((h', w'), (h, w)) = bounds as

buildSegTree :: (VUM.Unbox a) => a -> Int -> IO (VUM.IOVector a)
buildSegTree e n = VUM.replicate (n * 2) e

-- 一点更新
updateSegTree ::
  (VUM.Unbox a) =>
  (a -> a -> a) -> -- 演算
  VUM.IOVector a -> -- 木
  Int -> -- n (要素数)
  Int -> -- i (0-indexed)
  a -> -- 新しい値
  IO ()
updateSegTree op vec n i x = do
  let !pos = n + i
  VUM.write vec pos x
  updateParent (pos `div` 2)
  where
    updateParent !p
      | p < 1 = return ()
      | otherwise = do
          l <- VUM.read vec (2 * p)
          r <- VUM.read vec (2 * p + 1)
          VUM.write vec p (op l r)
          updateParent (p `div` 2)

-- 区間クエリ [l, r)
querySegTree ::
  (VUM.Unbox a) =>
  (a -> a -> a) -> -- 演算
  a -> -- 単位元
  VUM.IOVector a -> -- 木
  Int -> -- n (要素数)
  Int -> -- l (0-indexed)
  Int -> -- r (0-indexed, 含まない)
  IO a
querySegTree op e vec n l r = go (l + n) (r + n) e
  where
    go !l' !r' !acc
      | l' >= r' = return acc
      | otherwise = do
          acc1 <-
            -- 奇数であれば右の子であるのでその値を採用する
            if odd l'
              then op acc <$> VUM.read vec l'
              -- そうでなければ親の値を使う。偶数の場合右に兄弟ノードがあるため、その値を含めないと正しい値が分からない
              else return acc
          acc2 <-
            if odd r'
              -- queryは半開区間[)なので-1でr'を含まないようにする
              then op acc1 <$> VUM.read vec (r' - 1)
              else return acc1
          -- lが奇数である場合は値を参照したので、親の兄弟に遷移したい（木で見ると右上の親）
          -- 子iの親はi/2+1。divは切り捨てなのでl'+1を2で割っても同じ計算。5`div`2+1==6`div`2
          -- rは偶奇に関わらず親に向かう。rは含まれない範囲
          go ((l' + 1) `div` 2) (r' `div` 2) acc2

bfs ::
  -- 隣接リストのグラフ
  Array Int [Int] ->
  -- start
  Int ->
  -- 頂点とそこへのstartからの距離
  UArray Int Int
bfs graph start = runST $ do
  dist <- newArray (bounds graph) (-1) :: ST s (STUArray s Int Int)
  -- スタート地点のコストは0
  writeArray dist start 0

  -- queueは再帰で引き回すのでrefで持たない
  go (Seq.singleton start) dist
  freeze dist
  where
    go Seq.Empty dist = return ()
    -- queueの先頭要素の抜き出し
    go (u Seq.:<| queue) dist = do
      -- 現在の距離を取得
      d <- readArray dist u
      -- 隣接頂点の処理。graphは隣接リストなので、(graph ! u)が隣接した頂点一覧になる
      -- step dで現在の頂点に固定して、qと隣接頂点一覧を受けている
      queue' <- foldM (step d) queue (graph ! u)
      -- 更新したqueueを元に再帰
      go queue' dist
      where
        step d q v = do
          dv <- readArray dist v
          -- 未訪問の頂点だったら、現在の距離+1で更新。その頂点をenqueueする
          if dv == -1
            then do
              writeArray dist v (d + 1)
              return (q Seq.|> v)
            -- 訪問済みならそのまま
            else return q

buildGraph :: (Int, Int) -> [[Int]] -> Array Int [Int]
-- flipは関数の引数の順序を入れ替える。x:xsは第一引数を二つ目の配列の先頭に追加する演算子
buildGraph (i, n) uvs = accumArray (flip (:)) [] (i, n) xs
  where
    -- concatMapは配列の各要素に関数を適用した後に結合する。ここではu,vを前後入れ替えたtupleにしている
    -- accumArrayはtupleのfstをindexとして、値にtupleのsndを使う。同じindexに複数の値がある時、accumArrayの第一引数で結合する
    xs = concatMap (\[u, v] -> [(u, v), (v, u)]) uvs

buildWeightedGraph :: (Int, Int) -> [[Int]] -> Array Int [(Int, Int)]
buildWeightedGraph (i, n) uvcs = accumArray (flip (:)) [] (i, n) xs
  where
    xs = concatMap (\[u, v, c] -> [(u, (v, c)), (v, (u, c))]) uvcs

-- クラスカル法のためのsortしたedge
edges :: [[Int]] -> [(Int, Int, Int)]
edges uvcs = L.sortBy (comparing (\(w, _, _) -> w)) [(c, u, v) | [u, v, c] <- uvcs]

data UnionFind = UF
  { parent :: !(VUM.IOVector Int),
    rank :: !(VUM.IOVector Int)
  }

newUf :: Int -> IO UnionFind
-- newUf n = UF <$> VUM.generate (n + 1) id <*> VUM.replicate (n + 1) 0
newUf n = do
  -- 1basedの添え字で扱いたいのでn+1。個数が増える分には問題ない
  p <- VUM.generate (n + 1) id
  r <- VUM.replicate (n + 1) 0
  return (UF p r)

findUf :: UnionFind -> Int -> IO Int
findUf uf x = do
  -- parent[x]を読み込み
  p <- VUM.read (parent uf) x
  -- 自身が親なら終了
  if p == x
    then return x
    else do
      -- 再帰的に親に移動
      root <- findUf uf p
      -- 親が見つかったら自身の親を見つかった親に更新
      -- p <- ... <- xのように途中でなにか挟まっていると呼び出しが増えていってしまう
      VUM.write (parent uf) x root
      return root

uniteUf :: UnionFind -> Int -> Int -> IO Bool
uniteUf uf x y = do
  -- unionfindの同じグループかどうかの判定はrootが同じかどうかで判定される
  px <- findUf uf x
  py <- findUf uf y
  -- 同じなら特に何もせず終了
  if px == py
    then return False
    else do
      -- rankを読み取って、より小さい方に大きい方を繋ぐ
      rx <- VUM.read (rank uf) px
      ry <- VUM.read (rank uf) py
      case compare rx ry of
        LT -> VUM.write (parent uf) px py
        GT -> VUM.write (parent uf) py px
        EQ -> do
          -- 同じだったら片方に繋いで、ランクを1増やす
          -- writeは配列 index value の順に引数を受ける。parent ufが配列を返すのを忘れずに
          VUM.write (parent uf) py px -- pyをpxに繋ぐ
          VUM.modify (rank uf) (+ 1) px -- pxの子が増えたのでrankを+1
      return True

sameUf :: UnionFind -> Int -> Int -> IO Bool
-- sameUf uf x y = (==) <$> findUf uf x <*> findUf uf y
sameUf uf x y = do
  px <- findUf uf x
  py <- findUf uf y
  return (px == py)

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

main :: IO ()
main = do
  [n, m] <- ints
  abcs <- replicateM m ints

  uf <- newUf n
  -- 辺の重み昇順
  let sortedEdges = L.sortBy (comparing (\(_, _, c) -> c)) [(u, v, c) | [u, v, c] <- abcs]
      -- 終了条件が二つ。edgesがもうない。または頂点数(今回はn)-1本のedgeが作られた
      go [] _ totalWeight = return totalWeight
      go _ edgesLen totalWeight
        | edgesLen == n - 1 = return totalWeight
      go ((u, v, c) : edges) edgesLen totalWeight = do
        -- 接続！。uniteUfは繋いだ時にTrueを返す
        connected <- uniteUf uf u v
        if connected
          then go edges (edgesLen + 1) (totalWeight + c)
          -- 繋がらんかったら無視しましょう
          else go edges edgesLen totalWeight
  res <- go sortedEdges 0 0

  print res
