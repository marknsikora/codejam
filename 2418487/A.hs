import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

main = do
  tests <- readLn
  
  forM_ [1..tests] $ \caseNum -> do
    [r,t] <- map read . words <$> getLine

    -- let ans = length . takeWhile (<=(t::Int)) . scanl (\x y -> x + 2*y + 1) 0 $ [r,r+2..]
    let ans = (squareRoot (4*r^2-4*r+8*t+1)-2*r+1) `div` 4

    printf "Case #%d: %d\n" (caseNum::Int) (ans)

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
 
squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
     let twopows = iterate (^!2) 2
         (lowerRoot, lowerN) = 
           last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
         newtonStep x = div (x + div n x) 2
         iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
         isRoot r  =  r^!2 <= n && n < (r+1)^!2
     in  head $ dropWhile (not . isRoot) iters
