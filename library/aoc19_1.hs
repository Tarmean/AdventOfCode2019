module Aoc19_1 where

main = do
   c <- readFile "input1"
   let d = map read $ lines c
       s0 = sum $ map fuelMass $ d
       s1 = day1b c
   print s0
   print s1

fuelMass :: Double -> Double
fuelMass = (subtract 2) . fromIntegral . floor . (/3)

fuelFix s = go s 0
  where
    go c acc 
       | c <= 0 = acc
       | otherwise = go (fuelMass c) (acc + c)
go c acc 
   | c <= 0 = acc
   | otherwise = go (fuelMass c) (acc + c)
day1b = foldr (go . fuelMass . read) 0 . lines
