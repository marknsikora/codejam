import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    _ <- getLine
    
    x <- map read . words <$> getLine
    y <- map read . words <$> getLine

    let ans = sum $ zipWith (*) (reverse . sort $ x) (sort y)

    printf "Case #%d: %d\n" (caseNum::Int) (ans::Int)
