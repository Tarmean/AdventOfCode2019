module Aoc19_10 where
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (minimumBy, maximumBy, sortOn, unfoldr)
import Data.Ord
import Control.Arrow


type Point = (Int, Int)
parse :: String -> S.Set Point 
parse = S.fromList . concatMap parseRow . zip [0..] . lines
  where parseRow (i,l) = [(j,i) | (j, '#') <- zip [0..] l]

normalize :: Point -> Point
normalize (a,b) 
  | z /= 0 = (a `div` z, b `div` z)
  | otherwise = (a,b)
  where z = gcd a b

from :: Point -> Point -> Point
from = step
  where step (l,r) (a,b) = (a-l, b-r)
  
buildMap :: Point -> [Point] -> M.Map Point [Point]
buildMap p = M.map (sortOn f) . M.fromListWith (<>) . map (normalize.from p &&& (:[]))
  where
    f = manhattan . from p
    manhattan (x,y) = abs x + abs y

countPos :: Point -> S.Set Point -> Int
countPos p = pred . S.size . S.map (normalize . from p)

angle :: Point -> Double
angle(x,y)
  | a < 0 = a + 360
  | a >= 360 = a - 360
  | otherwise = a
  where
    base = atan2 (fromIntegral y) (fromIntegral x) / pi * 180
    a = negate (base - 90)

sortAngles :: M.Map Point [Point] -> [Point]
sortAngles = sortOn r . M.keys
  where r (x,y) = angle (x,-y)

data S = S [Point] (M.Map Point [Point])
stepS :: S -> Maybe (Point, S)
stepS (S (x:xs) m) = let (a, m') = step x m in Just (a, S xs m')
stepS (S [] m)
  | (x:xs) <- sortAngles m = stepS (S (x:xs) m)
  | otherwise = Nothing
step :: Point -> M.Map Point [Point] -> (Point, M.Map Point [Point])
step p m = case m M.! p of
   [a] -> (a, M.delete p m)
   (a:xs) -> (a, M.insert p xs m)

steps :: M.Map Point [Point] -> [Point]
steps m = unfoldr stepS (S [] m) 


main = do
  let s = maximumBy (comparing (flip countPos ls)) $ S.toList $ ls
      p = (countPos s ls)
  print s
  print p
  let m = buildMap s $ S.toList $ S.delete s ls
      l = steps m
  print $ l !! 199
      
ls = parse $ unlines [
    "##.##..#.####...#.#.####",
    "##.###..##.#######..##..",
    "..######.###.#.##.######",
    ".#######.####.##.#.###.#",
    "..#...##.#.....#####..##",
    "#..###.#...#..###.#..#..",
    "###..#.##.####.#..##..##",
    ".##.##....###.#..#....#.",
    "########..#####..#######",
    "##..#..##.#..##.#.#.#..#",
    "##.#.##.######.#####....",
    "###.##...#.##...#.######",
    "###...##.####..##..#####",
    "##.#...#.#.....######.##",
    ".#...####..####.##...##.",
    "#.#########..###..#.####",
    "#.##..###.#.######.#####",
    "##..##.##...####.#...##.",
    "###...###.##.####.#.##..",
    "####.#.....###..#.####.#",
    "##.####..##.#.##..##.#.#",
    "#####..#...####..##..#.#",
    ".##.##.##...###.##...###",
    "..###.########.#.###..#."]
