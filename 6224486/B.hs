import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

import GHC.Exts

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    _ <- getLine
    d <- map read <$> words <$> getLine

    let ans = reduce 0 (sortWith Down (d::[Int]))
          where
            reduce n [] = n
            reduce n xss@(x:xs)
              | x < 4 = reduce (succ n) (sortWith Down . filter (>0) . map pred $ xss)
              | otherwise = reduce (succ n) (sortWith Down $ (d+r):d:xs)
              where
                (d, r) = divMod x 2

    printf "Case #%d: %d\n" (caseNum::Int) (ans::Int)
