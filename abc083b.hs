import Data.Char (digitToInt)
ints :: IO [Int]
ints = map read . words <$> getLine

digitSum n = sum $ map digitToInt $ show n

main :: IO ()
main = do
    [n, a, b] <- ints
    let res = sum [ni | ni <- [1..n], let s = digitSum ni, s >= a && s <= b]
    print res
