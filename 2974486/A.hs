import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    n <- readLn
    a <- replicateM 4 $ map read . words <$> getLine

    m <- readLn
    b <- replicateM 4 $ map read . words <$> getLine

    let ans = (a !! (n-1)) `intersect` (b !! (m-1))

    printf "Case #%d: %s\n" (caseNum::Int) $ case (ans::[Int]) of
      [] -> "Volunteer cheated!"
      [x] -> show x
      _ -> "Bad magician!"
