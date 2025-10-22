{-# LANGUAGE LambdaCase #-}
import Data.Char (digitToInt, isSpace)
import Data.List (sort, isSuffixOf, isPrefixOf, unfoldr, nub)
import Control.Monad (replicateM, msum, forM_)
import Control.Monad.RWS (MonadState(put))
import qualified Data.Set as S
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

main :: IO ()
main = do
    [n] <- ints
    as <- ints
    let sorted = sort $ S.toList (S.fromList as)
        n' = length sorted
        sorted' = listArray @UArray (0, n') sorted
    putStrLn $ unwords $ map (\a -> show $ (+1) $ bisect (\i -> (sorted' ! i) >= a) n' (-1)) as
