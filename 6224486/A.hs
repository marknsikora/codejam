import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

import qualified Data.Char as Char


main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    [_, s] <- words <$> getLine

    let s' = map Char.digitToInt s

    let ans = reduce 0 0 0 s'
          where
            reduce k _ _ [] = k
            reduce k a n (x:xs)
              | a < n = reduce (succ k) (a + x + 1) (succ n) xs
              | otherwise = reduce k (a + x) (succ n) xs

    printf "Case #%d: %d\n" (caseNum::Int) (ans::Int)
