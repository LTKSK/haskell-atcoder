import Data.Maybe (catMaybes)
items = [(1, 100), (2, 200), (3, 300)]

main :: IO ()
main = do
    allChoices <- mapM (\item -> [Nothing, Just item]) items
    let validChoices = filter isValid (map catMaybes allChoices)
      where
        isValid chosen = sum (fst chosen) <= maxWeight
    putStr validChoices
