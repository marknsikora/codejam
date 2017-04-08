import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    [s, k] <- words <$> getLine

    k' <- readIO k

    let ans = solve s' where
          s' = map (=='+') s

          solve [] = Just 0
          solve xs | length xs < k' && not (all id xs) = Nothing
          solve xs = case span id xs of
                       ([], _) -> let (ys, yss) = splitAt k' xs
                                   in succ <$> solve (map not ys ++ yss)
                       (_, ys) -> solve ys

    printf "Case #%d: %s\n" (caseNum::Int) (maybe "IMPOSSIBLE" show ans)
