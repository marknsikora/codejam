import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    [n,m] <- map read . words <$> getLine

    l <- replicateM n $ map read . words <$> getLine

    let check = any (\(i,j) -> l!!i!!j < horzMax!!j && l!!i!!j < vertMax!!i) ps where
          ps = [(i,j) | i <- [0..n-1], j <- [0..m-1]]
          vertMax = map maximum l
          horzMax = map maximum . transpose $ (l::[[Int]])

    let ans = case check of
          True  -> "NO"
          False -> "YES"

    printf "Case #%d: %s\n" (caseNum::Int) ans
