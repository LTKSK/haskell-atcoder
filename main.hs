import qualified Data.Vector.Unboxed as VU
import Data.Array
import qualified Data.Set as S

main :: IO ()
main = do
    -- Vector
    let v = VU.fromList [1, 2, 3, 4, 5] :: VU.Vector Int
    print $ VU.sum v

    -- Array
    let arr = listArray (0, 4) [10, 20, 30, 40, 50] :: Array Int Int
    print $ arr ! 2

    -- Set
    let s = S.fromList [1, 2, 3, 2, 1]
    print s
