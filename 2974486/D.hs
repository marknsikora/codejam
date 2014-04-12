import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Ix
import Data.List
import Text.Printf

import qualified Data.Set as Set

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    _ <- getLine
    naomi <- Set.fromList . map (read . padBlock) . words <$> getLine
    ken <- Set.fromList . map (read . padBlock) . words <$> getLine

    let optimal = fst $ solve (0,0) naomi ken
        deceitful = snd $ solve (0,0) ken naomi

        solve :: (Int,Int) -> Set.Set Int -> Set.Set Int -> (Int,Int)
        solve points x y
          | Set.null x = points
          | Set.null y = points
          | maxX > maxY = solve (succPoints _1) nextX (Set.deleteMin y)
          | otherwise = solve (succPoints _2) nextX (Set.deleteMax y)
            where
              (maxX, nextX) = Set.deleteFindMax x
              maxY = Set.findMax y
              succPoints l = over l succ points

    printf "Case #%d: %d %d\n" (caseNum::Int) (deceitful::Int) (optimal::Int)

padBlock :: String -> String
padBlock s = go (drop 2 s) [1..5] where
  go (x:xs) (_:ys) = x : go xs ys
  go [] (y:ys) = '0' : go [] ys
  go _ _ = []
