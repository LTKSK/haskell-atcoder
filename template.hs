-- !で正格評価を強制
{-# LANGUAGE BangPatterns #-}
-- doやcaseに渡す関数のカッコを省略できる
{-# LANGUAGE BlockArguments #-}
-- \case -> が書けるようになる
{-# LANGUAGE LambdaCase #-}
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
import Data.Bool
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt, intToDigit, isSpace, ord)
import Data.Containers.ListUtils
import Data.Heap qualified as H
import Data.IORef
import Data.Int (Int64)
import Data.IntMap.Strict qualified as IM
import Data.Ix
import Data.List qualified as L
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord
import Data.STRef
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Debug.Trace
import Numeric (showIntAtBase)

-- デバッグ用
dbg :: (Show a) => a -> ()
dbg = (`traceShow` ())

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

integers :: IO [Integer]
integers = L.unfoldr (BS.readInteger . BS.dropWhile isSpace) <$> BS.getLine

chars :: IO [Char]
chars = filter (not . isSpace) . BS.unpack <$> BS.getLine

toBinStr :: Int -> String
toBinStr n = showIntAtBase 2 intToDigit n ""

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

-- Int と残りのByteStringを返す
intStr :: IO (Int, BS.ByteString)
intStr = do
  line <- BS.getLine
  let ws = BS.words line
  case ws of
    [n, s] -> case BS.readInt n of
      Just (num, _) -> return (num, s)
      Nothing -> error "数字のパースに失敗"
    _ -> error "フォーマットが違う"

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
-- concatで多次元配列を1次元配列に
getMatInt h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h ints

getMatChar :: Int -> Int -> IO (UArray (Int, Int) Char)
-- concatで多次元配列を1次元配列に
getMatChar h w = listArray ((1, 1), (h, w)) . concat <$> replicateM h getLine

-- 基数変換。nをbase進数に変換。戻り値はL.foldl' (\acc v -> acc*10+v) 0 とかで畳めば1つの値に出来る
toBaseDigits :: (Integral a) => a -> a -> [a]
toBaseDigits base 0 = [0]
toBaseDigits base n = reverse $ go n
  where
    go 0 = []
    go x = (x `mod` base) : go (x `div` base)

binSearch :: (Int -> Bool) -> Int -> Int -> Int
binSearch f ok ng
  | abs (ok - ng) <= 1 = ok
  | otherwise =
      let mid = (ok + ng) `div` 2
       in if f mid
            then binSearch f mid ng -- 条件を満たすならmidをokに
            else binSearch f ok mid -- 逆はngをmidに

-- 条件を満たすものがなかった時にNothingを返す。なんもなかったら初期値を使いたい場合なんかを想定
binSearch' :: (Int -> Bool) -> Int -> Int -> Maybe Int
binSearch' f !ok !ng
  | abs (ok - ng) <= 1 = if f ok then Just ok else Nothing
  | f mid = binSearch' f mid ng -- midがok
  | otherwise = binSearch' f ok mid -- midがng
  where
    mid = (ok + ng) `div` 2

-- tuple拡張
instance (Num a) => Num (a, a) where
  (x1, x2) + (y1, y2) = (x1 + y1, x2 + y2)
  (x1, x2) - (y1, y2) = (x1 - y1, x2 - y2)
  (x1, x2) * (y1, y2) = (x1 * y1, x2 * y2)
  negate (x1, x2) = (negate x1, negate x2)
  abs (x1, x2) = (abs x1, abs x2)
  signum (x1, x2) = (signum x1, signum x2)
  fromInteger n = (fromInteger n, fromInteger n)

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

-- 参考: https://zenn.dev/osushi0x/articles/e5bd9fe60abee4
shakutori ::
  (a -> b -> Int -> Bool) -> -- 条件p
  (b -> a -> b) -> -- 右端を伸ばす演算op
  (b -> a -> b) -> -- 左端を縮める演算invOp
  b -> -- 初期値 identyty
  [a] -> -- 入力のリスト
  [Int] -- 戻り値。条件を満たす部分列の長さのリスト
shakutori p op invOp identity as = go as as 0 identity
  where
    -- 右端が空になった際は左を詰める
    go lls@(l : ls) [] len res = len : (go ls [] (len - 1) (invOp res l))
    go lls@(l : ls) rrs@(r : rs) len res
      -- 条件を満たしたら伸長
      | p r res len = go lls rs (len + 1) (op res r)
      | len == 0 = 0 : (go ls rs 0 identity)
      -- 条件を満たさない場合は短縮。短縮はlsを渡すことで実現している
      -- 引数のasはどちらも同じ内容なので、lやrはlとrのポインタに相当する。添え字排除のためにこうなんだと思う
      | otherwise = len : (go ls rrs (len - 1) (invOp res l))
    go _ _ _ _ = []

-- もじゅーら計算
modulus :: Int
modulus = 1_000_000_007

addMod, subMod, mulMod :: Int -> Int -> Int
addMod x y = (x + y) `mod` modulus
subMod x y = (x - y) `mod` modulus
mulMod x y = (x * y) `mod` modulus

-- 高速べき乗
powMod :: Int -> Int -> Int -> Int
powMod a b m
  | b == 0 = 1
  | even b = powMod ((a * a) `mod` m) (b `div` 2) m
  | otherwise = (a * powMod a (b - 1) m) `mod` m

invMod :: Int -> Int
invMod x = powMod x (modulus - 2) modulus

-- exEuclid a b は s * a + t * b == g となる (g, s, t) を返す
-- sがaの逆元として使える
exEuclid :: Int -> Int -> (Int, Int, Int)
exEuclid a b = loop 1 0 0 1 a b
  where
    loop s t s' t' a b
      | b == 0 = (a, s, t)
      | otherwise = case a `divMod` b of
          (q, r) -> loop s' t' (s - q * s') (t - q * t') b r

invMod' :: Int -> Int
invMod' a = case exEuclid a modulus of
  (1, s, _) -> s `mod` modulus
  (-1, s, _) -> (-s) `mod` modulus
  _anyOfFailure -> error $ show a ++ " has no inverse modulo" ++ show @Int modulus

newtype IntMod = IntMod Int deriving (Eq)

instance Show IntMod where
  show (IntMod x) = show x

instance Num IntMod where
  IntMod x + IntMod y = IntMod ((x + y) `rem` modulus) -- 非負の場合remの方が高速だそう
  IntMod x - IntMod y = IntMod ((x - y) `mod` modulus)
  IntMod x * IntMod y = IntMod ((x * y) `rem` modulus) -- 非負の場合remの方が高速だそう
  fromInteger n = IntMod (fromInteger (n `mod` fromIntegral modulus))
  negate (IntMod x) = IntMod ((modulus - x) `mod` modulus)
  abs = undefined
  signum = undefined

instance Fractional IntMod where
  IntMod x / IntMod y = IntMod ((x * invMod y) `mod` modulus)

  -- 逆元：1/x=x^(-1)
  recip (IntMod x) = IntMod (invMod x)
  fromRational = undefined

fact :: Int -> IntMod
fact n = product [IntMod i | i <- [1 .. n]]

nCr :: Int -> Int -> IntMod
nCr n r = fact n / (fact r * fact (n - r))

-- combination
comb :: Int -> Int -> Int
comb n m = product [n - m + 1 .. n] `div` product [1 .. m]

-- log2 kの値を返す
log2LE :: Int -> Int
log2LE k = floor (logBase 2 (fromIntegral k) :: Double)

-- b: 状態空間の範囲、k: あり得る移動回数の最大値、f: 1回の状態遷移(arr ! とかを投げよう)
doubling ::
  (IArray a v, Ix v) =>
  (v, v) ->
  Int ->
  (v -> v) ->
  Array Int (a v v)
doubling bnd k f =
  listArray (0, maxK) $
    -- iterate'が無限listを返すので必要な分までtake。0から始まるのでmaxK+1
    take (maxK + 1) $
      L.iterate'
        -- ダブリング配列を作っている
        -- 一つ前の場所から2回遷移。その次は2回遷移の2回遷移なので4回遷移。と2^maxK回遷移する
        (\dp -> genArray bnd (\v -> dp ! (dp ! v)))
        -- 初期値になる配列を作っている。indexにfが適用される。as ! を投げるとasまんまになる
        (genArray bnd f)
  where
    maxK = log2LE k

-- k: 移動回数
-- v0: スタート地点
-- k回はkを2進数に変換して、bitが1の時の値を足し合わせると作れるので、
-- testBitで0basedのi番目のbitが立っているかをチェック
doublingQuery :: (IArray a v, Ix v) => Array Int (a v v) -> Int -> v -> v
doublingQuery dp k v0 = L.foldl' step v0 [0 .. maxK]
  where
    (_, maxK) = bounds dp
    step v i
      | testBit k i = (dp ! i) ! v
      | otherwise = v

-- 要素の出現回数をカウントする
-- toBucket (1, 5) [1, 2, 2, 3] => array [(1,1), (2,2), (3,1), (4,0), (5,0)]
toBucket :: (Ix i) => (i, i) -> [i] -> UArray i Int
toBucket b xs = accumArray @UArray (+) (0 :: Int) b $ map (,1) xs

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

-- ref: https://atcoder.jp/contests/abc439/submissions/72180327
-- 高速平方根計算
floorSqrt :: Int -> Int
floorSqrt n
  | n < 0 = error "Negative Root"
  | n == 0 = 0
  | otherwise =
      let !k = 32 - unsafeShiftR (countLeadingZeros (n - 1)) 1
          !x0 = unsafeShiftL 1 k
          !x1 = unsafeShiftR (x0 + unsafeShiftR n k) 1
       in go x0 x1
  where
    go !x !x'
      | x' < x = go x' (unsafeShiftR (x' + quot n x') 1)
      | otherwise = x

-- vectorを使った高速なsort
fastSortU :: (VUM.Unbox a, Ord a) => VU.Vector a -> VU.Vector a
fastSortU = VU.modify (VAI.sortBy compare)

fastSortLU :: (VUM.Unbox a, Ord a) => [a] -> [a]
fastSortLU = VU.toList . fastSortU . VU.fromList

fastSortDescLU :: (VUM.Unbox a, Ord a) => [a] -> [a]
fastSortDescLU = VU.toList . VU.modify (VAI.sortBy (comparing Down)) . VU.fromList

fastSortByLU :: (VUM.Unbox a) => (a -> a -> Ordering) -> [a] -> [a]
fastSortByLU ordering = VU.toList . fastSortByU ordering . VU.fromList

fastSortByU :: (VUM.Unbox a) => (a -> a -> Ordering) -> VU.Vector a -> VU.Vector a
fastSortByU ordering = VU.modify (VAI.sortBy ordering)

-- ランレングス圧縮
-- sort済みの配列を受け取る必要あり
-- L.groupを実行した結果の、[(要素,個数)]を返す
runLength :: (Eq a) => [a] -> [(a, Int)]
runLength = map f . L.group
  where
    f xs@(x : _) = let !len = length xs in (x, len)

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

-- indexの値を取得したいとき用
getSegTree ::
  (VUM.Unbox a) =>
  VUM.IOVector a -> -- 木
  Int -> -- n (要素数)
  Int -> -- i (0-indexed)
  IO a
getSegTree seg n i = VUM.read seg (n + i)

lrud = [(0, -1), (0, 1), (-1, 0), (1, 0)]

dfs :: Array Int [Int] -> Int -> UArray Int Bool
dfs graph start = runST $ do
  -- ST sのsはスレッドを区別するための幽霊型だそう。STUArrayはMutableなUnboxedの配列
  -- newArrayだけだとunboxedかboxedかを判断できないので型注釈が必要
  visited <- newArray (bounds graph) False :: ST s (STUArray s Int Bool)
  go visited start
  freeze visited
  where
    go visited u = do
      vis <- readArray visited u
      unless vis $ do
        writeArray visited u True
        -- 隣接ノードを探索
        forM_ (graph ! u) $ \v -> go visited v

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

buildGridGraphEdges h w =
  -- 右への辺
  [[v, v + 1] | y <- [1 .. h], x <- [1 .. w - 1], let v = (y - 1) * w + x]
    -- 下の辺
    ++ [[v, v + w] | y <- [1 .. h - 1], x <- [1 .. w], let v = (y - 1) * w + x]

-- 参考: https://atcoder.jp/contests/typical90/submissions/44161342
bfs01 ::
  (Ix a) =>
  (a -> [(a, Int)]) -> -- 隣接する頂点aとそのコスト
  (a, a) -> -- bounds
  [a] -> -- 開始地点複数
  UArray a Int -- 最小のコストを持つArray
bfs01 nextStates bnds v0s = runSTUArray $ do
  -- 最小コストをかんがえるので、maxBoundで初期化し、コストの低い方に緩和していく
  dist <- newArray bnds maxBound
  forM_ v0s $ \v0 -> writeArray dist v0 0
  go (Seq.fromList v0s) dist
  return dist
  where
    go Seq.Empty _ = return ()
    go (v Seq.:<| queue) dist = do
      d <- readArray dist v
      queue' <-
        foldM
          ( \q (u, w) -> do
              d' <- readArray dist u
              -- wがuに遷移した際のコストなので、uへの遷移が現在の値より安く済むなら更新
              if d + w < d'
                then do
                  writeArray dist u (d + w)
                  -- コストが1なら末尾。0なら先頭に配置
                  return $ bool (u Seq.<| q) (q Seq.|> u) (w == 1)
                else return q
          )
          queue
          (nextStates v)
      go queue' dist

data Dir = U | D | L | R | None deriving (Eq, Ix, Ord, Bounded)

-- グリッド用のラッパー
bfs01Grid :: UArray (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Int
bfs01Grid grid start goal = minimum [dist ! (fst goal, snd goal, d) | d <- [U, D, L, R]]
  where
    ((r0, c0), (r1, c1)) = bounds grid
    bnds3d = ((r0, c0, U), (r1, c1, R))
    v0s = [(fst start, snd start, d) | d <- [U, D, L, R]]

    dist = bfs01 nextStates bnds3d v0s

    nextStates (r, c, dir) =
      [ ((nr, nc, ndir), cost)
        | (dr, dc, ndir) <- [(-1, 0, U), (1, 0, D), (0, -1, L), (0, 1, R)],
          let nr = r + dr
              nc = c + dc
              cost = if ndir == dir then 0 else 1,
          inRange (bounds grid) (nr, nc),
          grid ! (nr, nc) /= '#'
      ]

dijkstra ::
  -- 隣接リストのグラフ。buildWeightedGraphで作るような重み付き
  Array Int [(Int, Int)] ->
  -- start
  Int ->
  -- 頂点とそこへのstartからの距離
  UArray Int Int
dijkstra graph start = runST $ do
  dist <- newArray (bounds graph) maxBound :: ST s (STUArray s Int Int)
  -- スタート地点のコストは0
  writeArray dist start 0

  -- queueは再帰で引き回すのでrefで持たない
  go dist (H.singleton (H.Entry 0 start))
  freeze dist
  where
    go dist heap
      | H.null heap = return ()
      | otherwise = do
          let Just (H.Entry cost u, heap') = H.viewMin heap
          currDist <- readArray dist u
          if cost > currDist
            then go dist heap'
            else do
              -- foldMで現在のheapを更新する
              -- やることは隣接頂点への重みを更新
              heap'' <- foldM (relax u cost) heap' (graph ! u)
              go dist heap''
      where
        relax u cost heap (v, c) = do
          dv <- readArray dist v
          let newCost = cost + c
          if newCost < dv
            then do
              -- 隣接頂点への重みを更新
              writeArray dist v newCost
              return $ H.insert (H.Entry newCost v) heap
            else return heap

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

-- cjpさんの https://atcoder.jp/contests/tessoku-book/submissions/69812807 を参考に
-- Dinicグラフ
data DinicGraph m
  = DinicGraph
  { -- 頂点数
    nvDG :: Int,
    -- 頂点毎の連結リスト
    graphDG :: VM.MVector (PrimState m) (VUM.MVector (PrimState m) EdgeDG)
  }

-- 行先、残余容量、逆辺が隣接リストの何番目か
type EdgeDG = (Int, Int, Int)

emptyDG :: (PrimMonad m) => Int -> m (DinicGraph m)
emptyDG n = do
  g <- VUM.new 0
  graph <- VM.replicate n g
  return $ DinicGraph n graph

addEdgeDG :: (PrimMonad m) => DinicGraph m -> (Int, Int, Int) -> m ()
addEdgeDG (DinicGraph nv g) (s, t, cap) = do
  gs <- VM.read g s
  gt <- VM.read g t
  -- s,tそれぞれの頂点の隣接リストを取得。次に追加するedgeのindexのために、lengthを取得
  let lenS = VUM.length gs
  let lenT = VUM.length gt
  -- 更新後のデータを作る。(0,0,0)は直後に上書きされるので何でもよさそう
  -- growでvectorを伸長する。この未初期化の値も直後に上書きされる
  gs' <- if lenS == 0 then VUM.replicate 1 (0, 0, 0) else VUM.grow gs 1
  gt' <- if lenT == 0 then VUM.replicate 1 (0, 0, 0) else VUM.grow gt 1
  -- 順方向。s->tの辺を追加。逆辺は、tのlenT番目にある
  VUM.write gs' lenS (t, cap, lenT)
  -- 逆方向。t->sの辺を追加。順辺は、sのlenS番目にある
  VUM.write gt' lenT (s, 0, lenS)
  -- 更新した内容を書き戻し
  VM.write g s gs'
  VM.write g t gt'

buildGraphDG :: (PrimMonad m) => Int -> [(Int, Int, Int)] -> m (DinicGraph m)
buildGraphDG n edges = do
  adjLists <- VM.replicate n ([] :: [EdgeDG])
  forM_ edges $ \(u, v, cap) -> do
    lu <- VM.read adjLists u
    lv <- VM.read adjLists v
    -- 逆辺のindexを参照。
    let ru = length lv -- これから頂点vに追加する辺のindexはlvのlengthになる。lvが2つだったらlengthは2、これは追加後のindexになっている
        rv = length lu -- 同上。逆向き
        -- 辺を追加
    VM.write adjLists u ((v, cap, ru) : lu) -- 順方向
    VM.write adjLists v ((u, 0, rv) : lv) -- 逆方向
    -- n頂点分のグラフを作成。辺は無し
  graph <- VM.new n
  forM_ [0 .. n - 1] $ \i -> do
    es <- VM.read adjLists i -- i番目のedge一覧を取得
    -- VM.write adjLists uのところで先頭に要素を追加していた。実際は末尾に追加されてほしいのでここでreverseしている
    -- ThawはFreezeの逆で、不変->可変に変換する処理
    -- thawと違ってコピーが発生しないので高速
    vec <- VU.unsafeThaw $ VU.fromList (reverse es)
    VM.write graph i vec

  return $ DinicGraph n graph

-- Dinic法では、まずbfsでレベルグラフを作る
-- このグラフでは最短距離を計算し、容量が残っていてかつ、通る辺の本数が最小になるものを利用する
-- 次にdfsを複数回行い、レベルグラフ上の経路で流せなくなるまで流す
maxFlowDG :: (PrimMonad m) => DinicGraph m -> (Int, Int) -> m Int
maxFlowDG (DinicGraph n graph) (s, t) = do
  level <- VUM.replicate n (-1 :: Int)
  iter <- VUM.replicate n 0

  -- levelグラフを構築する。探索先の管理にはSeqをqueueとして使う
  let bfs_ = do
        VUM.set level (-1)
        VUM.write level s 0 -- 開始地点のレベルを0に
        let go Seq.Empty = return () -- queueが空になったら終了
            go (v Seq.:<| q) = do
              -- 先頭から頂点を取り出し
              -- その頂点のレベルを取得
              lv <- VUM.read level v
              -- その頂点のedgeを取得
              edges <- VM.read graph v
              let len = VUM.length edges
              -- edgesをぐるぐる回す
              let loop i acc
                    | i == len = return acc
                    | otherwise = do
                        (to, cap, _) <- VUM.read edges i
                        -- 向かう先のレベルを取得
                        lto <- VUM.read level to
                        -- ここまでcapは初期値のままなので、順辺のみを探索する。レベルが<0な辺は未探索の辺
                        if cap > 0 && lto < 0
                          then do
                            -- レベルを現在の頂点+1
                            VUM.write level to (lv + 1)
                            -- 次のindexに移動。探索先にtoを追加
                            loop (i + 1) (acc Seq.|> to)
                          else loop (i + 1) acc
              q' <- loop 0 q
              go q'
        go (Seq.singleton s)

  -- 頂点とここまでのpathで流せる最大の流量
  let dfs_ v up
        -- ゴールの頂点に到達したら流量を返却
        | v == t = return up
        | otherwise = do
            edges <- VM.read graph v
            -- 次に見る頂点
            idx <- VUM.read iter v
            let len = VUM.length edges
            -- 各辺の処理
            let loop i
                  | i == len = return 0
                  | otherwise = do
                      -- iterに次に見る頂点として現在の頂点を保存。失敗した時に戻ってこないように更新される
                      -- idxの値はloopの引数に入っていて、idx..lenの範囲のみを探索
                      VUM.write iter v i
                      -- 順方向のedgeの情報を取得
                      (to, cap, rev) <- VUM.read edges i
                      lV <- VUM.read level v
                      lT <- VUM.read level to
                      -- 容量が残っていて、レベルが増加する方向のedge
                      if cap > 0 && lV < lT
                        then do
                          -- 流量と容量の小さい方を使って再帰。これで容量はどんどこ減っていく
                          d <- dfs_ to (min up cap)
                          -- 流せた場合
                          if d > 0
                            then do
                              -- edgeのcapを減らす
                              VUM.write edges i (to, cap - d, rev)
                              -- toのrev番目にアクセスすると逆向きの辺が入っている
                              revEdges <- VM.read graph to
                              (backTo, backCap, backRev) <- VUM.read revEdges rev
                              -- d分capを減らしたら、逆辺の容量を増やす
                              VUM.write revEdges rev (backTo, backCap + d, backRev)
                              -- 流した流量を返却
                              return d
                            else loop (i + 1)
                        else loop (i + 1)
            loop idx

  let loopFlow !total = do
        bfs_
        -- レベルグラフをbfs_で作った後にゴール地点のレベルを取得。これが-1だと到達できない
        l <- VUM.read level t
        if l < 0
          -- 流せるだけ流したということなので終了
          then return total
          else do
            -- iterの参照先を0にリセット
            VUM.set iter 0
            let sendFlow !acc = do
                  flow <- dfs_ s maxBound
                  if flow > 0
                    -- 流した分追加して再帰
                    then sendFlow (acc + flow)
                    -- 今回のループで流せた量の合計
                    else return acc
            -- 流した量を取得。0の時はもう流せなかったということで終了
            -- そうでないなら流した量を設定しつつもう一周
            pushed <- sendFlow 0
            if pushed == 0 then return total else loopFlow (total + pushed)

  loopFlow 0

-- nの桁の値を集計
digitSum :: Int -> Int
digitSum n = sum $ map digitToInt $ show n

-- nの約数を列挙
divisors :: Int -> [Int]
divisors n = go 1
  where
    go i
      | i * i > n = []
      | i * i == n = [i]
      | n `rem` i == 0 = i : (n `div` i) : go (i + 1)
      | otherwise = go (i + 1)

-- tuple
fst3 (a, _, _) = a

snd3 (_, b, _) = b

thd3 (_, _, c) = c

-- 表示系
yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

printArray2D :: (Show a, Ix i, Ix j, IArray arr a) => arr (i, j) a -> IO ()
printArray2D arr = do
  let ((r0, c0), (r1, c1)) = bounds arr
      rows = range (r0, r1)
      cols = range (c0, c1)
  forM_ rows $ \i ->
    putStrLn $ unwords [show (arr ! (i, j)) | j <- cols]

main :: IO ()
main = do
  print ""
