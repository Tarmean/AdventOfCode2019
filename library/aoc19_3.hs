{-# Language QuasiQuotes #-}
module Aoc19_3 where
import Control.Lens
import Data.Function (on)
import Control.Lens.Regex.Text 
import Data.List (scanl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M

step :: (Int, Int) -> Char -> (Int, Int)
step (x,y) 'R' = (x+1,y)
step (x,y) 'L' = (x-1,y)
step (x,y) 'D' = (x,y-1)
step (x,y) 'U' = (x,y+1)
step _ _  = undefined

collect :: [(Char, Int)] -> [((Int, Int), Int)]
collect = tail . flip zip [0..] . scanl' step (0,0) . concatMap f
  where f (c,i) = replicate i c

parse :: Text -> [(Char ,Int)]
parse t = t ^.. [regex|([RLDU])(\d+)|] . groups . to p
  where p = \[l,r] -> (T.head l, read (T.unpack r))

intersections :: Text -> Text -> M.Map (Int, Int) Int
intersections = M.intersectionWith (+) `on` M.fromList . collect . parse

shortest, closest :: (Ord a, Num a) => M.Map (a, a) a -> a
closest = minimum . map manhattan . M.keys
  where manhattan (a, b) = abs a + abs b
shortest = minimum . map snd . M.toList

main :: IO ()
main = do
    c <- T.readFile "library/input3"
    let [a,b] = T.lines c
    let ls = intersections a b
    print $ closest ls
    print $ shortest ls
