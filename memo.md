# setup

- pyenvで3.11.9のvenv作る
- activateして、pip install online-judge-tools 
- oj-setup.sh {問題のURL}
- ojth（自前alias）でtest
- ojs でsubmit

# memo

- ガード節はboolで分岐する場合
- caseは値で分岐する場合。配列要素の3番目がほにゃららとかでも捕まえられてよい
- catMaybes [m1, m2, m3]でモナドを繋げることができる。選んだ時、選ばなかった時をJust/Nothingで表現してその組み合わせ一覧を得る

## Array

- boundsで添え字の範囲を確認
- elems で一つの配列にした結果が見れる
- assocsでは添え字と値が確認できる
- listArray (1, n) のようにすると添え字を1basedにできる
- UArrayはプリミティブ型じゃないとダメ。タプルとかはArrayに入れる

### Arrayの//演算子

```haskell
arr // [(i, val), ...]
```
のような指定で、Arrayの値を更新できる  
Arrayの状態を遷移させていくdpで使いどころアリ。Listだとindexアクセスが重いから出番は結構あるかもしれない


## パターンマッチ的な構文覚える

```haskell
-- 以下の形でパターンマッチが書ける
resolve :: Int -> String
resolve n
  | odd n = ...
  | otherwise = ...
```

## enumerate

```haskell
zip [0..] arr
-- [(i, v)]

## dp

Haskellで戦う競技プログラミングを参考に、  
状態の遷移とそこまでの計算結果を参照する関数として、まずフィボナッチ数列を考える  
他の言語だと可変配列を使ったりするが、Haskellの場合はListや末尾再帰で実現できる

- Listの場合
  - zipWith (+) に、フィボナッチ数列のList、そのListをtailしたものを投げると実現できる
    - n+1の値は(n-1)+nで求まるので。ずらして足す
- 末尾再帰の場合
  - そもそも途中の結果はなくとも、最後の2つの計算結果が分かればよい
- 2つ前の要素と現在の要素の演算で、indexを操作するのではなく、zipで同じ配列をずらしてくっつけて実現できるのが面白い
```haskell
zip3 arr (tail arr) (tail $ tail arr)
```

漸化式なので、状態がどう遷移するかイメージするのが肝要  

- dp_a
  - 必要な情報を意識して、その遷移を考えられるのが大事
  - この問題だと今いる柱、1つ前の柱、2つ前の柱、1つ前の柱からのコスト、2つ前の柱からのコストの5つから、最少のコストを求める
- tessoku_r
  - dp[i] :: Bool
  - dp[i][j] :: Bool -- i枚目までで値jが作れるか
  - A[i] :: Bool -- カードの値
  - dp[i+1][j] = dp[i][j] or dp[i][j-A[i]]
    - これが漸化式
    - A[i]を選んだ場合と選ばなかった場合のいずれか

## List

### Listモナドとdo記法

doの中で
```
do
  v <- [0..(length arr)]
  return v
```
のように書ける。

### 内包表記

```haskell
[v | v <- [0..10]] --0,1,2,3,4,5,6,7,8,9,10
```
Pythonで見慣れたあいつ  
ガード節を複数書くことができて、短絡評価が可能。知らないとビビる  
0..10の指定は閉区間。Rustと一緒

```haskell
[v | v <- [0..10], v < 5, a <- [0..v], a > 3]
```
こんな感じで2重forっぽいのも書ける