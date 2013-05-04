import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

main = do
  tests <- readLn
  
  forM_ [1..tests] $ \caseNum -> do
    [a,n] <- map read . words <$> getLine
    m <- sort . map read . words <$> getLine

    let ans = grow a m where
          grow _ [] = 0
          grow 0 all = length all
          grow 1 all = length all
          grow x all@(y:ys)
            | y < x = grow (x+y) ys
            | best > length all = length all
            | otherwise = best
              where
                best = 1 + grow x ((x-1):all)

    printf "Case #%d: %d\n" (caseNum::Int) (ans::Int)
