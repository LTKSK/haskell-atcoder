
{-# LANGUAGE LambdaCase #-}
import Data.Char (digitToInt, isSpace)
import Data.List (sort, isSuffixOf, isPrefixOf, unfoldr)
import Data.Set(fromList, toList)
import Control.Monad (replicateM, msum, forM_, when)
import Control.Monad.RWS (MonadState(put))
import qualified Data.ByteString.Char8 as BS
import Data.Ix
import Data.Array.IArray
import Data.Array(Array)
import Data.Array.Unboxed(UArray)

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
-- concatで多次元配列を1次元配列に
getMatInt h w = listArray ((0,0), (h-1, w-1)) . concat <$> replicateM h ints

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
intwo =
    ints >>= \case
        [x, y] -> return (x,y)
        _ -> error "引数足りない"

main :: IO ()
main = do
    [n,q] <- ints
    as <- ints
    -- scanlで0の要素が増えるので、listArrayの範囲に注意
    -- l,rもlは-1で指定の日より一日前。rはそのままである
    let as' = listArray @UArray (0, n) $ scanl (+) (0 :: Int) as
    ps <- replicateM q $ do
        [l,r] <- ints
        return (l,r)

    forM_ ps $ \(l,r) -> do
        print $ as' ! r - as' ! (l-1)
