import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

import qualified Data.Set as Set

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    l <- reverse <$> getLine

    let ans = solve 0 True $ map (=='+') l
          where
            solve a _ [] = a
            solve a b (x:xs) | b == x = solve a b xs 
                             | otherwise = solve (a+1) (not b) xs

    printf "Case #%d: %d\n" (caseNum::Int) (ans::Int)
