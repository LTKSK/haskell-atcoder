import Data.Char (digitToInt)
import Data.List (sort, isSuffixOf, isPrefixOf)
import Data.Set(fromList, toList)
import Control.Monad (replicateM, msum)
import Control.Monad.RWS (MonadState(put))

ints :: IO [Int]
ints = map read . words <$> getLine
getints :: Int -> IO [Int]
getints n = replicateM n readLn :: IO [Int]

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix s
    | suffix `isSuffixOf` s = Just (take (length s - length suffix) s)
    | otherwise = Nothing

main :: IO ()
main = do
    s <- getLine
    -- putStrLn $ if canMake ["dreamer", "eraser", "dream", "erase"] s
    --     then "YES" else "NO"
    --   where
    --     stripSuffix' suffix s
    --         | suffix `isSuffixOf` s = Just (take (length s - length suffix) s)
    --         | otherwise = Nothing

    --     tryRemove ps s = msum [stripSuffix' p s | p <- ps]
    --     canMake _ "" = True
    --     canMake ps s = case tryRemove ps s of
    --         Nothing -> False
    --         Just s' -> canMake ps s'
    putStrLn $ if validate (reverse s) then "YES" else "NO"
        where
          validate "" = True
          validate s
              | "remaerd" `isPrefixOf` s = validate (drop 7 s)
              | "resare"  `isPrefixOf` s = validate (drop 6 s)
              | "maerd"   `isPrefixOf` s =  validate (drop 5 s)
              | "esare"   `isPrefixOf` s =  validate (drop 5 s)
              | otherwise = False
