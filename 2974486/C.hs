import Control.Applicative
import Control.Monad
import Data.Ix
import Data.List
import Text.Printf

intSquareRoot :: Int -> Int
intSquareRoot n = aux n
  where
    aux x
      | x*x > n = aux (x - 1)
      | otherwise = x

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    [r, c, m] <- map read . words <$> getLine

    let ans = board where
          open = (r*c) - m

          board = case boards (r,c) ++ map transpose (boards (c,r)) of
            [] -> Nothing
            (x:_) -> Just x

          boards (l,w) = map render box where
            box = [(x,y) | y <- [1..l],
                           let x = (open+y-1) `div` y,
                           x <= w,
                           x /= 1 || w == 1,
                           y /= 1 || l == 1,
                           x /= 2 || w == 2 || x*y == open,
                           y /= 2 || l == 2 || x*y == open,
                           x*y - open /= y-1]
            render (mx,my) = [[draw (x,y) | x <- [1..w]] | y <- [1..l]]
              where
                draw (1,1) = 'c'
                draw (x,y)
                  | x < mx && y <= my = '.'
                  | x == mx && (x-1)*my + y <= open = '.'
                  | otherwise = '*'

    printf "Case #%d:\n" (caseNum::Int)
    putStr $ case ans of
      Nothing -> "Impossible\n"
      Just board -> unlines board
