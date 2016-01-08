import Control.Applicative
import Control.Monad
import Data.List
import Data.Ord
import Text.Printf

import Text.ParserCombinators.Parsec hiding ((<|>))

import qualified Data.Set as Set

eol = char '\n'
integer = read <$> many1 digit

with_count p = do
  n <- integer <* eol
  count n p

names =
  let name = many1 (letter <|> char '_')
  in (\a b -> [a, b]) <$> name <* spaces <*> name

testcase = with_count $ names <* eol

parser = with_count testcase <* eof

main = do
  contents <- getContents
  let testcases = case (parse parser "(stdin)" contents) of
        Left err -> error $ show err
        Right x  -> x

  forM_ (zip testcases [1..]) $ \ (pairs, caseNum) -> do
    let ans = solve groupA groupB (tail sortedPairs) where
          sortedPairs = sortBy (comparing head) . map sort $ pairs
          [left, right] = head sortedPairs
          groupA = Set.singleton left
          groupB = Set.singleton right

          solve _ _ [] = True
          solve a b (x:xs)
            | Set.member l a && Set.member r a = False
            | Set.member l b && Set.member r b = False
            | Set.member l a || Set.member r b = solve (Set.insert l a) (Set.insert r b) xs
            | Set.member r a || Set.member l b = solve (Set.insert r a) (Set.insert l b) xs
            | otherwise = solve (Set.insert l a) (Set.insert r b) xs || solve (Set.insert r a) (Set.insert l b) xs
              where
                [l, r] = x

    printf "Case #%d: %s\n" (caseNum::Int) (if ans then "Yes" else "No")
