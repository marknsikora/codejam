import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    b <- replicateM 4 getLine
    _ <- getLine

    let check c = any (all (\(i,j) -> good $ b!!i!!j)) ps where
          good x = x == c || x == 'T'
          ps = [[(i,j) | i <- [0..3]] | j <- [0..3]] ++
               [[(j,i) | i <- [0..3]] | j <- [0..3]] ++
               [[(i,i) | i <- [0..3]]] ++
               [[(i,3-i) | i <- [0..3]]]

    let ans = case () of
          _ | check 'X' -> "X won"
            | check 'O' -> "O won"
            | '.' `elem` concat b -> "Game has not completed"
            | otherwise -> "Draw"

    printf "Case #%d: %s\n" (caseNum::Int) ans
