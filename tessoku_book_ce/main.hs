{-# LANGUAGE LambdaCase #-}
import Data.Char (digitToInt, isSpace)
import Data.List (sort, isSuffixOf, isPrefixOf, unfoldr)
import Control.Monad (replicateM, msum, forM_)
import Control.Monad.RWS (MonadState(put))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
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

binSearch :: (Int -> Bool) -> Int -> Int -> Int
binSearch f ok ng
  | abs (ok - ng) <= 1 = ok
  | otherwise =
    let mid = (ok + ng) `div` 2
    in if f mid
        then binSearch f mid ng -- 条件を満たすならmidをokに
        else binSearch f ok mid -- 逆はngをmidに

main :: IO ()
main = do
    [n] <- ints
    as <- ints
    [q] <- ints
    lrs <- replicateM q ints

    let pairs = scanl (\(win,lose) a -> if a == 1 then (win+1, lose) else (win, lose+1) ) (0,0) as :: [(Int, Int)]
        arr = listArray (0, n) pairs :: Array Int (Int, Int)

    forM_ lrs $ \[l,r] -> do
        let (lw,ll) = arr ! (l-1)
            (rw,rl) = arr ! r
            lsub = rl - ll
            wsub = rw - lw
        putStrLn $ if lsub > wsub
            then
                "lose"
            else
                if lsub < wsub
                    then "win"
                    else "draw"
