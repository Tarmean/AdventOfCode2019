{-# Language QuasiQuotes #-}
{-# Language OverloadedStrings #-}
module Aoc19_12 where
import Control.Lens.Regex.Text
import Data.List (transpose)
import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as V
import qualified Data.HashSet as H

parse :: T.Text -> [[Int]]
parse = toListOf $ [regex|<x=(-?\d+), y=(-?\d+), z=(-?\d+)>|] . groups . to (map $ read . T.unpack)

tag a b = case compare a b of
  LT -> 1
  EQ -> 0
  GT -> -1

delta :: Int -> V.Vector Int -> Int
delta a v = V.sum $ V.map (tag a) v
stepAcc :: V.Vector Int -> V.Vector Int -> V.Vector Int
stepAcc accs poss = V.zipWith step poss accs
  where step p acc = acc + delta p poss

addAcc :: V.Vector Int -> V.Vector Int -> V.Vector Int
addAcc = V.zipWith (+)

stepPair :: (V.Vector Int, V.Vector Int) -> (V.Vector Int, V.Vector Int)
stepPair (pos, accel) = (pos', accel')
  where
    accel' = stepAcc accel pos
    pos' = addAcc pos accel'
main = print out
out = map solve dimp
  where
    solve p = iterate stepPair (p, v0) !! 100
dimp = V.fromList <$> (parse t)
v0 :: V.Vector Int
v0 = V.replicate (V.length $ head dimp) 0
t :: T.Text
t = "<x=-8, y=-10, z=0> <x=5, y=5, z=10> <x=2, y=-7, z=3> <x=9, y=-8, z=-3>"
solve2 :: Int -> Int
solve2 i = go 0 H.empty $ base i
  where
    go i h (x:xs)
      | H.member x h = i
      | otherwise = go (i+1) (H.insert x h) xs
base i = map pot2 $ iterate stepPair (dimp !! i,v0)
pot2 (a,b) = V.sum (V.map abs a) * V.sum (V.map abs b)
solve1 = fmap (\i -> base i !! 100) [0..3]

pot = out
    & each . each %~ V.toList
    & transposeOf (each . each)
    & each %~ pot1
    & sum
  where
    pot1 ls = sumOf (each . _1 . to abs) ls * sumOf (each . _2 . to abs) ls
