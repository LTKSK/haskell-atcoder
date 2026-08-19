# template.hs 関数リファレンス

競技プログラミング用テンプレートの関数を「いつ使うか」で引けるように整理したメモ。
「こういう問題・入力が来たらこれ」という観点で並べてある。

---

## 1. 入力パース系

読み込みでいちいち悩まないための関数群。入力の形を見て選ぶ。

| 関数 | シグネチャ | いつ使う |
|------|-----------|---------|
| `ints` | `IO [Int]` | 1行にスペース区切りの整数が並んでいるとき（最頻出） |
| `integers` | `IO [Integer]` | 値が `Int` (64bit) を超える可能性があるとき |
| `chars` | `IO [Char]` | 1行の文字列を1文字ずつのリストで欲しいとき |
| `intsN n` | `Int -> IO [Int]` | 「1行に整数1個」がN行続くときに、まとめて `[Int]` にする |
| `intwo` | `IO (Int, Int)` | `X Y` の2整数をタプルで受けたいとき |
| `charInt` | `IO (Char, Int)` | `A 13` のような「文字 数字」 |
| `intChar` | `IO (Int, Char)` | `13 A` のような「数字 文字」 |
| `intIntChar` | `IO (Int, Int, Char)` | `3 5 R` のような「数字 数字 文字」 |
| `intStr` | `IO (Int, BS.ByteString)` | 「数字 文字列」で、文字列側はそのまま欲しいとき |
| `getMatInt h w` | `Int -> Int -> IO (UArray (Int,Int) Int)` | H×W の整数グリッドを2次元配列で受ける |
| `getMatChar h w` | `Int -> Int -> IO (UArray (Int,Int) Char)` | H×W の文字グリッド（`#.`の迷路など） |

> グリッド系は `((1,1),(h,w))` の **1-based** で作られる点に注意。

---

## 2. 出力系

| 関数 | 用途 |
|------|------|
| `yn b` | `Bool -> "Yes"/"No"` の文字列変換 |
| `printYn b` | そのまま `Yes/No` を出力 |
| `printArray2D arr` | 2次元配列をスペース区切りで整形表示（デバッグ・答え出力） |
| `dbg x` | `traceShow` のラッパー。`dbg x` を `seq` 的に挟んでデバッグ出力 |
| `toBinStr n` | 整数を2進文字列に（`showIntAtBase`） |

---

## 3. 二分探索

「単調性のある述語 `f` を満たす境界を探す」系。バリエーションが多いので使い分けを明確に。

| 関数 | 返すもの | 境界の意味 |
|------|---------|-----------|
| `binSearch f ok ng` | `Int` | `ok`側=条件を満たす、`ng`側=満たさない。両端を指定して境界の`ok`を返す |
| `binSearch' f ok ng` | `Maybe Int` | 上と同じだが、条件を満たすものが無ければ `Nothing`。初期値を使いたいとき |
| `binSearchMin f left right` | `t` | **`f`を満たす最小値**。`left`は答えを含まない側、`right`は含む側 |
| `binSearchMax f left right` | `t` | **`f`を満たす最大値**。全部満たすと `right-1` を返すので範囲は要素数+1にする |

> 「答え自体を二分探索する（めぐる式二分探索）」ときは `binSearchMin/Max` が使いやすい。
> 「配列の添字を探す」ような素朴な場合は `binSearch`。

---

## 4. ソート・圧縮・集計

| 関数 | 用途 |
|------|------|
| `fastSortLU xs` | リストの高速昇順ソート（vector経由。標準の`sort`より速い） |
| `fastSortDescLU xs` | 高速降順ソート |
| `fastSortByLU cmp xs` | 比較関数指定の高速ソート（リスト版） |
| `fastSortU / fastSortByU` | 上のVector版（Vectorのまま扱いたいとき） |
| `runLength xs` | ランレングス圧縮 `[(要素,個数)]`。**ソート済み前提**。連続同一の集計に |
| `uniqueCount base xs` | 座標圧縮。`(ランク配列, ユニーク数)` を返す。値が大きく添字に使えないとき |
| `toBucket (lo,hi) xs` | 出現回数のバケット配列。値域が分かっているカウントに |
| `nubOrd` 系 | `Data.Containers.ListUtils` から。順序保ちつつ重複除去（import済み） |

---

## 5. 数論・modular演算

### mod計算
| 関数 | 用途 |
|------|------|
| `modulus` | 定数 `1_000_000_007` |
| `addMod / subMod / mulMod` | mod付き加減乗 |
| `powMod a b m` | `a^b mod m` の高速べき乗（繰り返し二乗法） |
| `invMod x` | フェルマーの小定理での逆元（modが素数のとき） |
| `invMod' a` | 拡張ユークリッドでの逆元（modが素数でなくても可） |
| `exEuclid a b` | 拡張ユークリッド互除法 `(g, s, t)`。`s*a + t*b = g` |

### IntMod 型
`newtype IntMod` は **演算子でそのままmod計算**できる型。`+ - * /` が自動でmodを取る。
DPやパスカルの三角形など「式の中で自然にmodしたい」ときに `Int` の代わりに使うと楽。

| 関数 | 用途 |
|------|------|
| `fact n` | `n!`（IntMod） |
| `nCr n r` | 二項係数（IntMod、逆元利用） |

### mod無しの整数系
| 関数 | 用途 |
|------|------|
| `comb n m` | mod無しの `nCm`（オーバーフローしない小さい範囲用） |
| `divisors n` | `n` の約数を全列挙（√n） |
| `sieve n` | エラトステネスの篩。`UArray Int Bool` で素数判定表 |
| `digitSum n` | 各桁の和 |
| `toBaseDigits base n` | `n` を base進数の桁リストに |
| `floorSqrt n` | 高速な床平方根（`floor . sqrt` の誤差を避ける） |

---

## 6. 組合せ・順列の列挙

| 関数 | 出力 | いつ使う |
|------|------|---------|
| `combinations k xs` | `xs` から `k` 個選ぶ全組合せ | 部分集合の全探索 |
| `permsUniq xs` | 重複なし順列（**ソート済み前提**） | 同じ要素を含む順列全探索 |
| `pickUniq xs` | `[(選んだ要素, 残り)]`（重複スキップ） | `permsUniq` の部品。1個取り出す全パターン |
| `rangeProduct [a,b,c]` | 各 `1..x` の直積 | 各次元の範囲が違う多重ループの全探索 |

---

## 7. bit操作 / BitSet

### 生のbit操作
| 関数 | 用途 |
|------|------|
| `lowestBit l` | 最下位の立っているbitだけ残す `l & (-l)` |
| `largestPow2AtMost n` | n以下の最大の2べき |
| `log2GE n` | `2^x >= n` となる最小 `x`（切り上げ） |
| `log2LE n` | `2^x <= n` となる最大 `x`（切り下げ） |
| `msb n` | 最上位bitの位置（0-based）= `log2LE` |
| `bitLength n` | bit長（`msb + 1`） |

### BitSet 型（bit集合を型で扱う）
状態を集合として持つbit全探索・bitDPで、可読性を上げたいとき。**1-based index**。

| 関数 | 用途 |
|------|------|
| `emptyBS / fullBS n` | 空集合 / 全要素集合 |
| `singletonBS i` | i番目だけの集合 |
| `fromListBS xs` | リストから集合 |
| `insertBS / deleteBS` | 要素の追加・削除 |
| `memberBS / notMemberBS` | 所属判定 |
| `unionBS / intersectionBS` | 和・積 |
| `sizeBS` | 要素数（popCount） |
| `toListBS` | 立っているbit位置のリスト（1-based） |
| `bitLengthBS` | bit長 |

---

## 8. 累積和・BIT・セグ木

### 累積和
| 関数 | 用途 |
|------|------|
| `csum2 as` | 2次元累積和。矩形和クエリの前処理 |

### BIT（Fenwick Tree）
一点加算・prefix和。主に **転倒数** に。
| 関数 | 用途 |
|------|------|
| `bitUpdate bit i v` | 位置iにv加算 |
| `bitQuery bit i` | 1..i の和 |
| `inversions xs` | 転倒数を直接計算（1-based想定、内部でBIT使用） |

### セグメント木（IOVector, 0-indexed, 半開区間）
一点更新・区間クエリ。演算 `op` と単位元 `e` を渡す汎用型。
| 関数 | 用途 |
|------|------|
| `buildSegTree e n` | 単位元 `e`、サイズ `n` で構築 |
| `updateSegTree op vec n i x` | 位置iを`x`に更新 |
| `querySegTree op e vec n l r` | 区間 `[l, r)` のクエリ |
| `getSegTree vec n i` | 位置iの値取得 |

> 使うたびに `op`/`e` を渡す設計。区間min→`op=min, e=maxBound`、区間和→`op=(+), e=0` など。

---

## 9. グラフ構築

| 関数 | 用途 |
|------|------|
| `buildGraph (i,n) uvs` | 無向グラフの隣接リスト（`[[u,v]]` から） |
| `buildWeightedGraph (i,n) uvcs` | 重み付き無向グラフ（`[[u,v,c]]` から） |
| `buildGridGraphEdges h w` | グリッドを頂点番号のグラフに変換した辺リスト |

---

## 10. グラフ探索

問題に応じて選ぶ。**辺の重みで使い分ける**のが基本。

| 関数 | いつ使う |
|------|---------|
| `dfs graph start` | 連結成分・到達可能性。訪問済み `UArray Int Bool` を返す |
| `bfs graph start` | **重みなし（=1）**の最短距離。距離配列を返す |
| `bfs01 next bnds v0s` | 辺の重みが **0 か 1** の最短路（両端キュー） |
| `bfs01Grid grid s g` | 方向転換コスト1のグリッド最短（`bfs01`のグリッド版） |
| `bfsGrid dist grid queue` | グリッドBFS（IOUArray使用。複数始点も可） |
| `dijkstra graph start` | **正の重み**の最短路（ヒープ使用） |

> 迷うとき: 重みなし→`bfs`、0/1→`bfs01`、一般の正重み→`dijkstra`。

補助定数:
- `lrud` … 上下左右4方向の `(dy,dx)`
- `around` … 8方向

---

## 11. 最大流（Dinic法）

`s→t` の最大流。二部マッチングや最小カットにも帰着できる。
| 関数 | 用途 |
|------|------|
| `emptyDG n` | 頂点数nの空グラフ |
| `addEdgeDG g (s,t,cap)` | 辺を1本追加 |
| `buildGraphDG n edges` | 辺リストから一括構築 |
| `maxFlowDG g (s,t)` | 最大流を計算 |

---

## 12. ダブリング

「k回遷移した先」をlog(k)で求める。k が巨大なとき用。

| 関数 | 用途 |
|------|------|
| `doubling bnd k f` | 遷移関数`f`のダブリングテーブルを事前計算 |
| `doublingQuery dp k v0` | `v0` から `k` 回遷移した先 |
| `doublingWithAccum bnd k f val op` | 遷移＋道中の累積値（合計・max等）付き |
| `doublingWithAccumQuery dp k v0 zero op` | `(k回後の位置, 累積値)` |

---

## 13. DP汎用

配列ベースのDPを関数化したもの。状態遷移`f`・緩和`combine`を渡す。

| 関数 | 特徴 |
|------|------|
| `accumDP f combine ini bnds v0s xs` | 遷移先が無い状態を**前の値のまま引き継ぐ**（`accum`ベース）。ナップサック等 |
| `accumArrayDP next relax ini bnds v0s xs` | 毎回初期値で作り直す版（`accumArray`ベース） |

> 「選ぶ/選ばない」で毎ステップ全状態を更新するDPを、添字を書かずに書ける。

---

## 14. 尺取り法

| 関数 | 用途 |
|------|------|
| `shakutori p op invOp identity as` | 条件`p`を満たす部分列の長さリスト。右端伸長`op`・左端縮小`invOp` |

「条件を満たす連続部分列の最大長/個数」系に。

---

## 15. Union-Find

| 関数 | 用途 |
|------|------|
| `newUf n` | サイズn（1-based想定でn+1確保） |
| `findUf uf x` | 根を求める（経路圧縮あり） |
| `uniteUf uf x y` | 併合（rank/size管理）。新規併合なら`True` |
| `sameUf uf x y` | 同一グループ判定 |
| `sizeUf uf x` | xを含むグループのサイズ |

---

## 16. その他ユーティリティ

| 関数 | 用途 |
|------|------|
| `fst3 / snd3 / thd3` | 3タプルの要素取り出し |
| `instance Num (a,a)` | タプル同士の四則演算（座標の加減算などが `+` で書ける） |

---

### 使い分けの早見（問題タイプ → 関数）

- **最短経路**: 重みなし→`bfs` / 0-1→`bfs01` / 正重み→`dijkstra`
- **連結・グループ管理**: `UnionFind` 一式 / 単純な到達性は`dfs`
- **区間クエリ**: 静的な和→`csum2` / 動的更新あり→セグ木 or BIT
- **転倒数**: `inversions`
- **modありの数え上げ**: `IntMod` + `nCr`/`fact`
- **巨大なk回遷移**: `doubling`系
- **座標が大きすぎて添字にできない**: `uniqueCount`（座標圧縮）
- **全探索**: 部分集合→`combinations`/`BitSet` / 順列→`permsUniq` / 多重範囲→`rangeProduct`
- **最大流/マッチング**: Dinic (`maxFlowDG`)

---

## 付録: 引数が複雑な関数の呼び出しサンプル

「たまにしか使わないと引数の順番・意味を忘れる」関数を、実戦的な呼び出し例つきでまとめる。

### セグメント木（区間min / 区間和）

```haskell
-- 区間最小値（RMQ）。単位元は maxBound
main = do
  let n = 8
  seg <- buildSegTree (maxBound :: Int) n     -- e=maxBound, size=n
  updateSegTree min seg n 3 5                  -- 位置3(0-indexed)を5に
  updateSegTree min seg n 0 2                  -- 位置0を2に
  ans <- querySegTree min maxBound seg n 0 4   -- [0,4) の最小値
  print ans

-- 区間和にしたいなら op=(+), e=0 に変えるだけ
--   seg <- buildSegTree (0 :: Int) n
--   updateSegTree (+) seg n i x
--   querySegTree (+) 0 seg n l r
```

ポイント: `op` と `e` は **build/update/query で一貫**させる。区間min→`(min, maxBound)`、区間max→`(max, minBound)`、区間和→`((+), 0)`。

### 尺取り法 `shakutori`

```haskell
-- 「和が k 以下の連続部分列の最長長さ」を各左端について求める
--   p      : 右端 r を足しても条件を満たすか (a -> b -> Int -> Bool)
--   op     : 右端を伸ばす演算 (b -> a -> b)
--   invOp  : 左端を縮める演算 (b -> a -> b)
--   identity: 累積の初期値
solve k as = shakutori p (+) (-) 0 as
  where
    -- res=現在の区間和, r=新しく入る右端の値, _len=現在の長さ
    p r res _len = res + r <= k
-- 戻り値は各時点の部分列長 [Int]。maximum を取れば最長長さ
```

ポイント: `op`/`invOp` は「累積値 `b` に要素 `a` を出し入れする」演算。和なら `(+)`/`(-)`、積なら `(*)`/`div`。`p` の第3引数は現在の区間長。

### 畳み込みDP `accumDP`（0-1ナップサック）

```haskell
-- 状態 v = これまでの重み, 値 e = その重みでの最大価値
-- wvs = [(重み, 価値)] の入力リスト
knapsack maxW wvs =
  accumDP @UArray f max minBound (0, maxW) [(0, 0)] wvs
  where
    -- (現在の重み w, 現在価値 val) と 入力(wi, vi) を見て遷移先を返す
    f (w, val) (wi, vi)
      | w + wi <= maxW = [(w + wi, val + vi)]  -- 品物を採用
      | otherwise      = []                    -- 採用不可（=前の値を引き継ぐ）
```

ポイント: 引数順は `f combine 初期値 (下界,上界) 開始状態 入力`。`f` が空リストを返すと **その状態は前の値を維持**（`accum`ベースなので）。「選ばない」を明示的に書かなくてよいのが特徴。

### ダブリング `doubling` / `doublingQuery`

```haskell
-- arr ! i = 頂点iの1回の遷移先（0-indexed, 0..n-1）とする
prepare n arr = doubling @UArray (0, n - 1) maxK (arr !)
  where maxK = 30   -- 遷移回数kの上限。2^30 ≈ 10^9 まで対応

-- 準備したテーブル dp で「頂点 i から k 回遷移した先」
query dp k i = doublingQuery dp k i
```

累積値つき（k回移動の道中の合計コストも欲しいとき）:

```haskell
-- val i = 頂点iにいるときの値, op = 累積演算(ここでは和)
dp = doublingWithAccum @UArray (0, n-1) maxK (nxt !) (val !) (+)
(dst, total) = doublingWithAccumQuery dp k start 0 (+)
--   dst   = k回後の位置
--   total = 道中の累積値, zero=0 は op の単位元
```

ポイント: `bnd` は **遷移先も含めた値域** を渡す。`k` はクエリの最大移動回数（テーブルは `log2LE k` 段まで作られる）。

### 0-1 BFS `bfs01`

```haskell
-- nextStates v = [(隣接頂点, コスト0または1)]
-- v0s = 複数始点のリスト
dist = bfs01 nextStates ((0,0), (h,w)) [start]
  where
    nextStates (y, x) =
      [ ((y+dy, x+dx), cost)
      | (dy, dx, cost) <- moves
      , inRange bnds (y+dy, x+dx) ]
```

ポイント: コストは **0 か 1 のみ**（それ以外は`dijkstra`へ）。始点は**リスト**で渡すので多始点もそのまま書ける。

### Dinic 最大流

```haskell
maxFlow = runST $ do
  g <- buildGraphDG n edges       -- edges = [(from, to, cap)]
  maxFlowDG g (source, sink)
-- 逐次追加したいときは emptyDG n してから addEdgeDG g (s,t,cap) を繰り返す
```

ポイント: 頂点番号は **0-indexed**。二部マッチングは「超源点→左集合(cap1)→右集合→超汇点(cap1)」で構築して最大流を取る。

### めぐる式二分探索 `binSearchMin` / `binSearchMax`

```haskell
-- TODO(human): binSearchMin または binSearchMax の呼び出し例
```
