
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

main :: IO ()
main = do
    [d] <- ints
    [n] <- ints
    lrs <- replicateM n $ do
        [l,r] <- ints
        return (l,r)

    -- tupleの左辺がindexで右辺が値になる。これはaccumArrayの引数の型
    let updates = [(l,1) | (l,r) <- lrs] ++ [(r+1,-1) | (l,r) <- lrs, r < d] :: [(Int, Int)]
        -- tail2回は、accumArrayのindexを0から作っているのと、scanlの分
        res = accumArray (+) 0 (0, d) updates :: UArray Int Int
        res' = tail $ tail $ scanl (+) 0 (elems res)

    mapM_ print res'
