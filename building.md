# Haskellパッケージ環境構築手順

## 概要
競技プログラミング用のHaskell環境で、vectorなどの追加パッケージを使えるようにする手順。

## 背景
- このプロジェクトは個別の`.hs`ファイルを直接`ghc`でコンパイルする形式
- `cabal.project`や`stack.yaml`は使わない
- `ghc -O2 -o main main.hs`のような直接コンパイルでパッケージを使いたい

## 手順

### 1. vectorパッケージのインストール

```bash
cabal install --lib vector
```

### 2. 必要な標準パッケージの追加

vectorだけをインストールすると、GHC環境ファイルが`clear-package-db`を使って他のパッケージを隠してしまうため、必要な標準パッケージも明示的に追加する：

```bash
cabal install --lib array containers bytestring mtl
```

**必要なパッケージ:**
- `array` - Data.Array, Data.Array.Unboxedなど
- `containers` - Data.Map, Data.Setなど
- `bytestring` - Data.ByteString（高速入力用）
- `mtl` - モナド変換子ライブラリ
- `vector` - Data.Vector（高速配列）

### 3. 動作確認

環境ファイルの内容を確認：
```bash
cat ~/.ghc/x86_64-linux-9.6.7/environments/default
```

以下のような内容になっているはず：
```
clear-package-db
global-package-db
package-db /home/ltksk/.cabal/store/ghc-9.6.7/package.db
package-id base-4.18.3.0
package-id vector-0.13.2.0-...
package-id array-0.5.8.0
package-id bytestring-0.12.2.0-...
package-id containers-0.8-...
package-id mtl-2.3.1
```

テストコンパイル：
```bash
ghc -O2 -o main main.hs
```

### 4. VSCode設定

`.vscode/settings.json`に以下を追加：

```json
{
  "haskell.serverEnvironment": {
    "GHC_ENVIRONMENT": "/home/ltksk/.ghc/x86_64-linux-9.6.7/environments/default"
  }
}
```

**重要:** 設定追加後、VSCodeを再読み込みする：
- `Ctrl+Shift+P` → "Developer: Reload Window"

必要に応じてHLSも再起動：
- `Ctrl+Shift+P` → "Haskell: Restart Haskell LSP Server"

## 使用例

```haskell
import qualified Data.Vector.Unboxed as VU
import Data.Array
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    -- Vector (型注釈が重要!)
    let v = VU.fromList [1, 2, 3] :: VU.Vector Int
    print $ VU.sum v

    -- Array
    let arr = listArray (0, 2) [10, 20, 30] :: Array Int Int
    print $ arr ! 1

    -- Set
    let s = S.fromList [1, 2, 3, 2, 1]
    print s
```

## よく使うVector操作

```haskell
import qualified Data.Vector.Unboxed as VU

-- 作成
let v = VU.fromList [1,2,3] :: VU.Vector Int

-- アクセス
v VU.! 0  -- インデックス0の要素

-- 長さ
VU.length v

-- map/filter
VU.map (*2) v
VU.filter (>2) v

-- sum/maximum/minimum
VU.sum v
VU.maximum v
VU.minimum v

-- slice
VU.slice start len v  -- startからlen個の要素
```

## トラブルシューティング

### ArrayやSetが使えない
→ `cabal install --lib array containers`を実行

### VSCodeでエラーが消えない
1. VSCodeを再読み込み: `Ctrl+Shift+P` → "Developer: Reload Window"
2. HLSを再起動: `Ctrl+Shift+P` → "Haskell: Restart Haskell LSP Server"
3. `.vscode/settings.json`の設定を確認

### 型の曖昧性エラー
Vectorを使う時は型注釈を明示する：
```haskell
-- ❌ エラー
let v = VU.fromList [1,2,3]

-- ✅ OK
let v = VU.fromList [1,2,3] :: VU.Vector Int
```

## 注意点

- `cabal install --lib`で作成される環境ファイルは、明示的に追加したパッケージのみを有効にする
- 標準ライブラリでも、環境ファイルに含まれていないと"hidden package"として扱われる
- 新しいパッケージを追加したら、VSCodeを再読み込みすること

## パッケージの追加

新しいパッケージが必要になったら：

```bash
# 単一パッケージ
cabal install --lib パッケージ名

# 複数パッケージ
cabal install --lib package1 package2 package3

# 例: scipperパッケージを追加
cabal install --lib scipper
```

追加後、必ずVSCodeを再読み込み。
