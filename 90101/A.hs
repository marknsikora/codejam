import Control.Applicative
import Control.Monad
import Text.Printf

main = do
  [_, d, n] <- map read . words <$> getLine

  dict <- replicateM d getLine

  forM_ [1..n] $ \caseNum -> do
    word <- getLine
    
    let ans = length . filter match $ dict
          where match = all (\(x,y) -> y `elem` x) . zip (expand word)

    printf "Case #%d: %d\n" (caseNum::Int) (ans::Int)

expand [] = []
expand (x:xs)
  | x == '('  = pos : expand rest
  | otherwise = [x] : expand xs
  where (pos, ')':rest) = span (')'/=) xs
