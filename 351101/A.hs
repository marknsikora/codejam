import Control.Applicative
import Control.Monad
import Data.List
import Data.Tuple
import Text.Printf

main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    c <- readLn
    i <- readLn
    p <- map read . words <$> getLine

    let ans = head . filter (\(j,k) -> p!!j + p!!k == (c::Int)) $ ps where
          ps = [(j,k) | j <- [0..i-1], k <- [j..i-1], j /= k]

    printf "Case #%d: %d %d\n" (caseNum::Int) (1 + fst ans) (1 + snd ans)
