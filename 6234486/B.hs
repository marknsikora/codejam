import Control.Applicative
import Control.Monad
import Text.Printf

import qualified Data.Set as Set

main = do
  tests <- readLn

  forM_ [1..tests] $ \ caseNum -> do
    [v, d] <- map (fromIntegral . read) . words <$> getLine

    let ans = 90.0/pi * asin(d / (v*v) * 9.8)

    printf "Case #%d: %f\n" (caseNum::Int) (ans::Double)
