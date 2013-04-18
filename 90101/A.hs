import Control.Applicative
import Control.Monad
import Text.Printf

main = do
  [_, d, n] <- map read . words <$> getLine

  dict <- replicateM d getLine

  forM_ [1..n] $ \caseNum -> do
    word <- getLine
    
    let ans = length $ expandWithDict dict word

    printf "Case #%d: %d\n" (caseNum::Int) (ans::Int)

expand [] = [[]]
expand (x:xs)
  | x /= '('  = (x:) <$> expand xs
  | otherwise = (:) <$> pos <*> expand (tail rest)
  where (pos, rest) = span (')'/=) xs

expandWithDict _ [] = [[]]
expandWithDict [] _ = []
expandWithDict dict (x:xs)
  | x /= '('  = if not . null $ filterDict
                  then (x:) <$> expandWithDict (tail <$> filterDict) xs
                  else []
  | otherwise = concatMap (expandWithDict dict) ((:tail rest) <$> pos)
  where filterDict = filter (\ys -> x == head ys) dict
        (pos, rest) = span (')'/=) xs
