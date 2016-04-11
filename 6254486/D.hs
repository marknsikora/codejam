import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split
import Text.Printf

import qualified Data.Set as Set

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    [k,c,s] <- map read . words <$> getLine

    let ans | length paths > s = Nothing
            | otherwise = Just $ map followPaths paths
              where
                paths = chunksOf c $ [0..k-1]
                followPaths = succ . sum . zipWith (*) (iterate (*k) 1) . reverse

    printf "Case #%d: %s\n" (caseNum::Int) (maybe "IMPOSSIBLE" (unwords . map show) ans)
