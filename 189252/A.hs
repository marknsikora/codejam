import Control.Monad
import Data.List
import Data.Maybe
import Text.Printf

main = do
  tests <- readLn
  
  forM_ [1..tests] $ \ caseNum -> do
    str <- getLine
    
    let ans = foldl (\x y -> x*base + y) 0 . map digify $ str where
          digify c = case elemIndex c (nub str) of
            Just 0  -> 1
            Just 1  -> 0
            Just n  -> n
            Nothing -> error "The impossible happened!"
          base = succ . maximum . map digify $ str
    
    printf "Case #%d: %d\n" (caseNum::Int) (ans::Int)
