{-# LANGUAGE LambdaCase #-}
import Data.Char (digitToInt, isSpace)
import Data.List (sort, isSuffixOf, isPrefixOf, unfoldr, group)
import Control.Monad (replicateM, msum, forM_)
import Control.Monad.RWS (MonadState(put))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import Data.Ix
import Data.Array.IArray
import Data.Array(Array)
import Data.Array.Unboxed(UArray)
import qualified Data.Map.Strict as M
import GHC.Weak (finalize)

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
intwo =
    ints >>= \case
        [x, y] -> return (x,y)
        _ -> error "引数足りない"

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
-- concatで多次元配列を1次元配列に
getMatInt h w = listArray ((0,0), (h-1, w-1)) . concat <$> replicateM h ints

binSearch :: (Int -> Bool) -> Int -> Int -> Int
binSearch f ok ng
  | abs (ok - ng) <= 1 = ok
  | otherwise =
    let mid = (ok + ng) `div` 2
    in if f mid
        then binSearch f mid ng -- 条件を満たすならmidをokに
        else binSearch f ok mid -- 逆はngをmidに

fibStep :: (Int, Int) -> (Int, Int)
fibStep (fibN, fibNm1) = (fibN + fibNm1, fibN)
fib :: Int -> Int
fib n = case loop n (1, 0) of (fibNpp1, fibN) -> fibN
    where
        loop 0 state = state
        loop n state = loop (n-1) (fibStep state)

solve :: [Int] -> [Int] -> Int
solve (a1:as) bs = final
    where
        (final, _) = foldl step (a1, 0) (zip as bs)
        step (p1, p2) (a,b) = (min (p1+a) (p2+b), p1)

-- ref: https://zenn.dev/ppdx999/books/65e882010be891/viewer/a9377d
main :: IO ()
main = do
    [n] <- ints
    as <- ints
    bs <- ints
    print $ solve as bs
