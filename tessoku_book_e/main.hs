
{-# LANGUAGE LambdaCase #-}
import Data.Char (digitToInt, isSpace)
import Data.List (sort, isSuffixOf, isPrefixOf, unfoldr)
import Data.Set(fromList, toList)
import Control.Monad (replicateM, msum)
import Control.Monad.RWS (MonadState(put))
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)

modulus :: Int64
modulus = 10 ^ 9 + 7 -- 1_000_000_007
addMod :: Int64 -> Int64-> Int64
addMod x y = (x + y) `mod` modulus

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
intwo =
    ints >>= \case
        [x, y] -> return (x,y)
        _ -> error "引数足りない"

main :: IO ()
main = do
    [n,k] <- ints

    print $ length [() | a <- [1..n], b <- [1..n], let c = k-a-b, c <= n && c > 0]
