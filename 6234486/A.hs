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
    pairs <- replicateM n $ words <$> getLine

    let sortedPairs = sortBy (comparing head) . map sort $ pairs

    let ans = solve groupA groupB (tail sortedPairs) where
          [left, right] = head sortedPairs
          groupA = Set.singleton left
          groupB = Set.singleton right

          solve _ _ [] = True
          solve a b (x:xs)
            | Set.member l a && Set.member r a = False
            | Set.member l b && Set.member r b = False
            | Set.member l a || Set.member r b = solve (Set.insert l a) (Set.insert r b) xs
            | Set.member r a || Set.member l b = solve (Set.insert r a) (Set.insert l b) xs
            | otherwise = solve (Set.insert l a) (Set.insert r b) xs || solve (Set.insert r a) (Set.insert l b) xs
              where
                [l, r] = x

    printf "Case #%d: %s\n" (caseNum::Int) (if ans then "Yes" else "No")
