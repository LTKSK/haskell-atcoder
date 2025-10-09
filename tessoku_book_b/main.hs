
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

main :: IO ()
main = do
    (n,x) <- intwo
    as <- ints
    let res = length [() | a <- as, a == x]
    putStrLn $ if res > 0 then "Yes" else "No"
