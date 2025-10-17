
{-# LANGUAGE LambdaCase #-}
import Data.Char (digitToInt, isSpace)
import Data.List (sort, isSuffixOf, isPrefixOf, unfoldr)
import Data.Set(fromList, toList, foldl')
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

main :: IO ()
main = do
    [n] <- ints
    as <- ints
    [d] <- ints
    lrs <- replicateM d ints
    -- 累積和ならぬ累積maxで求めよう
    let toRight = listArray @UArray (1, n) $ tail $ scanl max 0 as
    let toLeft = listArray @UArray (1, n) $ reverse $ tail $ scanl max 0 (reverse as)
    forM_ lrs $ \[l,r] -> do
        let lmax = toRight ! (l-1)
        let rmax = toLeft ! (r+1)
        print $ max lmax rmax
