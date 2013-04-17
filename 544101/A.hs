import Data.List
import Control.Applicative
import Control.Monad
import Text.Printf

main = do
  tests <- readLn
  
  forM_ [1..tests] $ \ caseNum -> do
    [n,k] <- map read . words <$> getLine
    b <- replicateM n getLine
    
    let f = map falldown b
          where falldown s = let (x,y) = partition (/='.') s in y ++ x
    
    let check c = any (all (\(i,j) -> c == f !! i !! j)) ps where
          ps = [[(i,j+l) | l <- [0..k-1]] | j <- [0..n-k], i <- [0..n-1]] ++
               [[(j+l,i) | l <- [0..k-1]] | j <- [0..n-k], i <- [0..n-1]] ++
               [[(i+l,j+l) | l <- [0..k-1]] | j <- [0..n-k] , i <- [0..n-k]] ++
               [[(i+l,j-l) | l <- [0..k-1]] | i <- [0..n-k], j <- [k-1..n-1]]

    let ans = case () of
          _ | all check "RB" -> "Both"
            | check 'B' -> "Blue"
            | check 'R' -> "Red"
            | otherwise -> "Neither"

    printf "Case #%d: %s\n" (caseNum::Int) ans
