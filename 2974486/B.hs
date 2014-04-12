import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    [c, f, x] <- map read . words <$> getLine

    let ans = solve 2.0 where
          solve i
            | finish < next = finish
            | otherwise = (c / i) + solve (i + f)
            where
              finish = x / i
              next = (c / i) + (x / (i + f))

    printf "Case #%d: %.7f\n" (caseNum::Int) (ans::Double)
