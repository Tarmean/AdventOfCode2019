{-# Language QuasiQuotes #-}
module Aoc19_3 where
import Control.Lens
import Control.Lens.Regex.Text 
import Data.List (minimumBy)
import Data.Text (Text)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M

step :: Char -> (Int, Int) -> (Int, Int)
step 'R' (x,y) = (x+1,y)
step 'L' (x,y) = (x-1,y)
step 'D' (x,y) = (x,y-1)
step 'U' (x,y) = (x,y+1)
step _ _ = undefined

collect :: [(Char, Int)] -> [((Int, Int), Int)]
collect ls = zip (go (0,0) ls) [1..]
  where
    go _ [] = []
    go p ((c,i):xs)= ls <> go (last ls) xs
      where ls = take i (iterate (step c) (step c p))

parse :: Text -> [(Char ,Int)]
parse t = t ^.. [regex|([RLDU])(\d+)|] . groups . to (\[l,r] -> (T.head l,read (T.unpack r)))

intersections :: Text -> Text -> M.Map (Int, Int) Int
intersections l r = M.intersectionWith (+) (points l) (points r)
  where points = M.fromList . collect . parse

closest :: (Foldable t, Ord a, Num a) => t (a, a) -> (a, a)
closest ls = minimumBy (comparing manhattan) ls
  where manhattan (a, b) = abs a + abs b

shortest :: (Ord a, Num a) => M.Map (a, a) a -> ((a, a), a)
shortest ls = minimumBy (comparing snd) $ M.toList ls

main = do
    c <- T.readFile "library/input3"
    let [a,b] = T.lines c
    let ls = intersections a b
    print $ closest (M.keys ls)
    print $ shortest ls
