import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

import qualified Data.Set as Set

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    [k,c,s] <- map read . words <$> getLine

    let ans = if s < k then Nothing else Just solve
          where
            solve = map (f k c) [1..k]

            f _ 1 z = z
            f x y z = x^(y-1) * (z-1) + (f x (y-1) z)

    printf "Case #%d: %s\n" (caseNum::Int) (maybe "IMPOSSIBLE" (unwords . map show) ans)
