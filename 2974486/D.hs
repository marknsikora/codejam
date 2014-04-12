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
    naomi <- Set.fromList . map read . words <$> getLine
    ken <- Set.fromList . map read . words <$> getLine

    let optimal = fst $ solve (0,0) naomi ken
        deceitful = snd $ solve (0,0) ken (naomi::Set.Set Double)
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
