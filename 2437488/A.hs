import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import qualified Data.Set as Set
import Text.Printf

main = do
  tests <- readLn
  
  forM_ [1..tests] $ \caseNum -> do
    [w,_n] <- words <$> getLine
    
    let n = read _n

    let ans = sum . map (succ . length . remaining) . filter valid . tails $ w where
          remaining = drop n . concat . dropWhile (\z -> any isVowel z || length z < n) . groupBy ((==) `on` isVowel)
          valid = any (>=n) . map length . filter (not . any isVowel) . groupBy ((==) `on` isVowel)

    printf "Case #%d: %d\n" (caseNum::Int) (ans)

isVowel c = 
  let vowels = Set.fromList "aeiou"
  in c `Set.member` vowels
