import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Text.Printf

data Decimal = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Enum, Eq, Ord)

instance Show Decimal where
  show Zero  = "ZERO"
  show One   = "ONE"
  show Two   = "TWO"
  show Three = "THREE"
  show Four  = "FOUR"
  show Five  = "FIVE"
  show Six   = "SIX"
  show Seven = "SEVEN"
  show Eight = "EIGHT"
  show Nine  = "NINE"

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    s <- getLine

    let ans = map fromEnum <$> solve [Zero .. Nine] s where
          solve _ [] = Just []
          solve [] _ = Nothing
          solve (n:ns) ss = case remove (show n) ss of
            Nothing -> solve ns ss
            Just ss' -> case solve (n:ns) ss' of
              Nothing -> solve ns ss
              Just xs -> Just (n:xs)

          remove [] ys = Just ys
          remove (x:xs) ys = remove xs =<< remove' ys where
            remove' [] = Nothing
            remove' (z:zs) = if z == x then Just zs else (z:) <$> remove' zs

    printf "Case #%d: %s\n" (caseNum::Int) (concat . map show . fromMaybe [] $ ans)
