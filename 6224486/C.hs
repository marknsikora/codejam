import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

data Sign a = Pos a | Neg a
            deriving Eq

main :: IO ()
main = do
  tests <- readLn

  forM_ [1..tests] $ \caseNum -> do
    [l, x] <- map read <$> words <$> getLine

    s <- concat . replicate x <$> getLine

    let ans = solve "ijk" (Pos '1') $ map Pos s
          where
            solve [] _ [] = True
            solve [] _ _ = False
            solve _ _ [] = False
            solve (c:cs) n xs = let next = reduce c n xs
                                    done = solve cs (Pos '1') next
                                in if done
                                   then True
                                   else solve (c:cs) (Pos c) next

            reduce _ _ [] = []
            reduce c n (x:xs) = let val = n `quaternions` x in
                                 if val == Pos c
                                 then xs
                                 else reduce c val xs

    printf "Case #%d: %s\n" (caseNum::Int) (if ans then "YES" else "NO")

quaternions a b = case (a, b) of
                   (Pos x, Pos y) -> multiplicative x y
                   (Pos x, Neg y) -> inv $ multiplicative x y
                   (Neg x, Pos y) -> inv $ multiplicative x y
                   (Neg x, Neg y) -> multiplicative x y
  where
    inv (Pos x) = Neg x
    inv (Neg x) = Pos x
    multiplicative x y = case (x,y) of
                          ('1', '1') -> Pos '1'
                          ('1', 'i') -> Pos 'i'
                          ('1', 'j') -> Pos 'j'
                          ('1', 'k') -> Pos 'k'

                          ('i', '1') -> Pos 'i'
                          ('i', 'i') -> Neg '1'
                          ('i', 'j') -> Neg 'k'
                          ('i', 'k') -> Pos 'j'

                          ('j', '1') -> Pos 'j'
                          ('j', 'i') -> Pos 'k'
                          ('j', 'j') -> Neg '1'
                          ('j', 'k') -> Neg 'k'

                          ('k', '1') -> Pos 'k'
                          ('k', 'i') -> Neg 'j'
                          ('k', 'j') -> Pos 'i'
                          ('k', 'k') -> Neg '1'
