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
          grow x all@(y:ys)
            | x <= 1    = length all
            | y < x     = grow (x+y) ys
            | otherwise = let best = 1 + grow x ((x-1):all)
                          in min best (length all)

    printf "Case #%d: %d\n" (caseNum::Int) (ans::Int)
