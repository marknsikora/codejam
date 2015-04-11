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
            reduce n xss@(x:_)
              | d <= headNum = x + n
              | otherwise = reduce (n + headNum) (sortWith Down next)
              where
                (d, r) = divMod x 2
                headNum = length . head . group $ xss
                next = (concat . tail . group $ xss) ++
                       (replicate headNum d) ++
                       (replicate headNum (d+r))

    printf "Case #%d: %d\n" (caseNum::Int) (ans::Int)
