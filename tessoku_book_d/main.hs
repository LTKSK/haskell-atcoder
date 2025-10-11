
{-# LANGUAGE LambdaCase #-}
import Data.Char (digitToInt, isSpace)
import Data.List (sort, isSuffixOf, isPrefixOf, unfoldr)
import Data.Set(fromList, toList)
import Control.Monad (replicateM, msum)
import Control.Monad.RWS (MonadState(put))
import qualified Data.ByteString.Char8 as BS

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
intwo =
    ints >>= \case
        [x, y] -> return (x,y)
        _ -> error "引数足りない"

resolve :: Int -> String
resolve 0 = "0"
resolve 1 = "1"
resolve n
    -- 奇数の時
    | odd n = "1" ++ resolve (n `div` 2)
    -- 偶数の時
    | otherwise = "0" ++ resolve (n `div` 2)


main :: IO ()
main = do
    [n] <- ints
    let res = resolve n
    let rem = 10 - length res
    putStrLn $ replicate rem '0' ++ reverse res
