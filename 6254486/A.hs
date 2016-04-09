import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

import qualified Data.Set as Set

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    n <- readLn

    let ans = case n of
          0 -> Nothing
          _ -> Just solve
            where
              solve = solve' n $ Set.fromList (show n)

              solve' a s | Set.size s == 10 = a
              solve' a s = solve' (a+n) (s `Set.union` Set.fromList (show$a+n))

    printf "Case #%d: %s\n" (caseNum::Int) (maybe "INSOMNIA" show ans)
