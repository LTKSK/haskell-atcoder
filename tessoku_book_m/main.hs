
{-# LANGUAGE LambdaCase #-}
import Data.Char (digitToInt, isSpace)
import Data.List (sort, isSuffixOf, isPrefixOf, unfoldr)
import Data.Set(fromList, toList)
import Control.Monad (replicateM, msum, forM_)
import Control.Monad.RWS (MonadState(put))
import qualified Data.ByteString.Char8 as BS
import Data.Ix
import Data.Array.IArray
import Data.Array(Array)
import Data.Array.Unboxed(UArray)

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
intwo =
    ints >>= \case
        [x, y] -> return (x,y)
        _ -> error "引数足りない"

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
-- concatで多次元配列を1次元配列に
getMatInt h w = listArray ((0,0), (h-1, w-1)) . concat <$> replicateM h ints

bisect :: (Int -> Bool) -> Int -> Int -> Int
bisect f ok ng
  | abs (ok - ng) <= 1 = ok
  | otherwise =
    let mid = (ok + ng) `div` 2
    in if f mid
        then bisect f mid ng -- 条件を満たすならmidをokに
        else bisect f ok mid -- 逆はngをmidに

shakutori :: UArray Int Int -> Int -> Int -> Int
shakutori arr k len = go 0 1 0
    where
        go l r res
            | r == len = res
            | l >= r = res
            -- 条件を満たさない時はlを進める
            | (arr ! r) - (arr ! l) > k = go (l+1) r res
            -- lの地点で差が小さいということは、rに進んでいくいずれも差はK以下であるためこの計上
            | otherwise = go l (r+1) (res+(r-l))

main :: IO ()
main = do
    [n,k] <- ints
    as <- ints

    print $ shakutori (listArray @UArray (0, n-1) as) k n
