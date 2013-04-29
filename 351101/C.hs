import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Text.Printf

main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    str <- getLine

    let ans = concatMap insertPause . groupBy pauseNeeded $ numified where
          numified = map numify str
          pauseNeeded = (==) `on` head
          insertPause = intercalate " "

    printf "Case #%d: %s\n" (caseNum::Int) ans

numify c = replicate count digit where
  digit = case findIndex (c `elem`) dict of
    Just n  -> intToDigit n
    Nothing -> error "Invalid character"
  count = case elemIndex c =<< find (c `elem`) dict of
    Just n  -> n + 1
    Nothing -> error "Invalid character"
  dict = [" ","","abc","def","ghi","jkl","mno","pqrs","tuv","wxyz"]
