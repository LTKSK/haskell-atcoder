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