import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

import qualified Data.Char as Char

data Pancake = Happy | Blank
  deriving (Eq, Show)

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    [s, k] <- words <$> getLine

    k' <- readIO k :: IO Int

    let ans = solve s' where
          s' = map (\c -> if c=='+' then Happy else Blank) s
          inv Happy = Blank
          inv Blank = Happy

          solve [] = Just 0
          solve xs | length xs < k' && not (all (==Happy) xs) = Nothing
          solve xs = case span (==Happy) xs of
                       ([], _) -> let (ys, yss) = splitAt k' xs
                                   in succ <$> solve (map inv ys ++ yss)
                       (_, ys) -> solve ys

    printf "Case #%d: %s\n" (caseNum::Int) (maybe "IMPOSSIBLE" show ans)
