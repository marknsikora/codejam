import Control.Applicative
import Control.Monad
import Data.List
import Data.Ord
import Text.Printf

import qualified Data.Set as Set

main = do
  tests <- readLn

  forM_ [1..tests] $ \ caseNum -> do
    n <- readLn
    names <- replicateM n getLine

    let ans = solve 0 names where
          solve a [] = a
          solve a (x:xs:xss)
            | x > xs = solve (a+1) (x:xss)
            | otherwise = solve a (xs:xss)
          solve a (x:xs) = solve a xs

    printf "Case #%d: %d\n" (caseNum::Int) (ans::Int)
