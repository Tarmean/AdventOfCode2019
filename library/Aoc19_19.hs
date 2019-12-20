module Aoc19_19 where
import IntCode

main = do
  print solve1'
  print solve2'

binarySearch x y p = go x y
  where
    go x y
      | y > x = Left (x,y)
    go x y = case p k of
       GT -> go (k+1) y
       EQ -> Right k
       LT -> go x (k-1)
      where k = (x + y) `div` 2

solve2 = head $ dropWhile tooSmall $ zip slices (drop 100 slices)
  where
    tooSmall (x,y) = (bot x - (top y - 1)) < 100
solve1' = sum $ map toSize $ take 50 slices
  where
toSize st = max 0 ((min 49 (bot st)) - (top st-1))
slices = Step 0 0 0 : Step 0 (-1) 0 : Step 0 (-1) 0 : iterate stepStep stepStart
-- searchTop :: Int -> Comparison
searchTop i = binarySearch 0 1000 (check i)
  where
check i y = case step1 i y of
            1 -> case step1 i (y-1) of
                0 -> EQ
                1 -> LT
            0 -> GT
stepStart = Step 3 4 4
data Step = Step { index :: Int, bot :: Int, top :: Int }
 deriving Show
solve2' = go 0 0
  where
   go x y
     | not (1 == step1 (x+99) y) = go x (y+1)
     | not (1 == step1 x (y+99)) = go (x+1) y
     | otherwise = x * 10000 + y
stepStep :: Step -> Step
stepStep (Step index bot top) = Step index' bot' top'
  where
    index' = index+1
    bot' = go0 index' (1) (max bot top')
    top' = go0 index' (-1) top

appDir dir height = max 0 (dir + height)
go0 index dir height = case step1 index height of 
  1 -> go 1 dir height `appDir` (-dir)
  0 -> (go 0 (-dir) height)
  where
    go s dir height
      | step1 index height == s = go s dir (dir `appDir` height) 
      | otherwise = height
      

solve1 = length $ filter (==1) [step1 x y | x <- [0..49], y <- [0..49]]

step1 :: Int -> Int -> Int
step1 x y = head $ runP (runMachine program dat) [x,y]
dat = [ 109,424,203,1,21101,11,0,0,1106,0,282,21101,0,18,0,1105,1,259,1201,1,0,221,203,1,21102,1,31,0,1105,1,282,21102,38,1,0,1106,0,259,21002,23,1,2,22101,0,1,3,21102,1,1,1,21102,1,57,0,1106,0,303,1201,1,0,222,21002,221,1,3,20101,0,221,2,21101,0,259,1,21101,80,0,0,1106,0,225,21102,1,33,2,21102,91,1,0,1106,0,303,1202,1,1,223,20101,0,222,4,21101,0,259,3,21101,0,225,2,21102,225,1,1,21101,0,118,0,1106,0,225,20102,1,222,3,21101,0,22,2,21101,133,0,0,1105,1,303,21202,1,-1,1,22001,223,1,1,21102,148,1,0,1106,0,259,1201,1,0,223,21002,221,1,4,20102,1,222,3,21102,5,1,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21102,1,195,0,105,1,108,20207,1,223,2,20101,0,23,1,21102,-1,1,3,21101,0,214,0,1106,0,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,2102,1,-4,249,22101,0,-3,1,21201,-2,0,2,21202,-1,1,3,21101,250,0,0,1105,1,225,22101,0,1,-4,109,-5,2105,1,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2105,1,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,22101,0,-2,-2,109,-3,2106,0,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,22102,1,-2,3,21102,1,343,0,1106,0,303,1105,1,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,21201,-4,0,1,21101,0,384,0,1105,1,303,1106,0,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,22101,0,1,-4,109,-5,2106,0,0]
