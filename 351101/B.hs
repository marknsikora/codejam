import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    str <- words <$> getLine

    let ans = unwords . reverse $ str

    printf "Case #%d: %s\n" (caseNum::Int) ans
