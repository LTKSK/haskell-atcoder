import Data.Char (digitToInt)
import Data.List (sort)

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
    [n] <- ints
    xs <- ints
    let sorted = reverse $ sort xs
    let alice = sum [v | (i, v) <- zip [0..] sorted, even i]
    let bob = sum [v | (i, v) <- zip [0..] sorted, odd i]

    print $ alice - bob
