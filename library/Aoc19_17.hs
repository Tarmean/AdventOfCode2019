{-# Language TupleSections #-}
module Aoc19_17 where
import Grid
import Control.Lens
import Control.Monad
import IntCode
import qualified Data.Map as M
import qualified Data.IntSet as S
import Control.Applicative
import qualified Data.List as L
import Data.Ord

cover :: [Step] -> [([Step], [Step], [Step], [Char])]
cover inp = do
    let offsetsCovered = S.empty
    -- sA is a substring of the input data
    -- lA is a list of indices sA occurs at in the input
    [(sA, lA), (sB, lB), (sC, lC)] <- choices 3 (getData inp)
    -- check that none of the selected places is already covered & mark them as taken
    offsetsCovered <- markSelection (length sA) lA offsetsCovered
    -- repeat for b and c
    offsetsCovered <- markSelection (length sB) lB offsetsCovered
    offsetsCovered <- markSelection (length sC) lC offsetsCovered
    -- check that we generate the total input
    guard (S.size offsetsCovered == length inp)
    let keys = M.elems $ M.fromList (map (,'A') lA <> map (,'B')lB <> map (,'C') lC)
    pure (sA, sB, sC, keys)

data SuffixTree a = SuffixTree (M.Map a (SuffixTree a)) [Int]
  deriving Show

toSuffixTree :: Ord a => [a] -> SuffixTree [a]
toSuffixTree = foldr1 mergeSuffix . map toSingle . zip [0..] . L.tails
  where
   toSingle (i, xs) = foldr (singleton i) (SuffixTree M.empty [i]) . tail . L.inits $ xs
   singleton i a b = SuffixTree (M.singleton a b) [i]
   mergeSuffix (SuffixTree a i) (SuffixTree b j) = SuffixTree (mergeSuffixMaps a b) (i<>j)
   mergeSuffixMaps = M.unionWith mergeSuffix

suffixList :: SuffixTree a -> [(a, [Int])]
suffixList (SuffixTree m _) = concatMap (uncurry go) (M.toList m)
  where
    go k (SuffixTree m' i) =  (k, i) : concatMap (uncurry go) (M.toList m')

getData :: Ord a => [a] -> [([a], [Int])]
getData = L.sortBy (comparing $ \(a,b) -> Down $ min (length a) (length b)) . filter (p . length . fst) . suffixList . toSuffixTree
  where
   p _i = True -- i <= 6 && i >= 2

data Step = Step Char Int
  deriving (Eq, Ord, Show)

type MState = S.IntSet
markSelection :: Alternative m => Int -> [Int] -> MState -> m MState
markSelection len ls m 
  | any (`S.member` m) idxs = empty
  | otherwise = pure $ foldr S.insert m idxs
  where idxs = [j | i <- ls, j <- [i..i+len - 1]]

testData :: [Step]
testData = [Step 'R' 4, Step 'R' 12, Step 'R' 10, Step 'L' 12, Step 'L' 12, Step 'R' 4, Step 'R' 12, Step 'L' 12, Step 'R' 4, Step 'R' 12, Step 'L' 12, Step 'L' 8, Step 'R' 10, Step 'L' 12, Step 'L' 8, Step 'R' 10, Step 'R' 4, Step 'R' 12, Step 'R' 10, Step 'L' 12, Step 'L' 12, Step 'R' 4, Step 'R' 12, Step 'L' 12, Step 'R' 4, Step 'R' 12, Step 'L' 12, Step 'L' 8, Step 'R' 10, Step 'R' 4, Step 'R' 12, Step 'R' 10, Step 'L' 12]
choices :: Int -> [a] -> [[a]]
choices 0 _ = [[]]
choices i (x:xs) = fmap (x :) (choices (i-1) xs) <|> (choices i xs)
choices _ [] = []



printSolution (a,b,c,m) = mapM_ putStrLn [m, concatMap printStep a, concatMap printStep b,concatMap printStep c]
  where printStep (Step c i) = c : show i
main :: IO ()
main = do
   mapM_ printSolution (take 200 $ cover testData)
   -- putStrLn $ map toEnum $ runP (runMachine program code) []
   -- print $ countNodes mapData
   -- print $ last $ runP (runMachine (memory 0 .= 2 >> program) code) solve2
solve2 :: [Int]
solve2 = map fromEnum $ unlines $ [
            "A,B,A,B,C,C,B,A,B,C",
            "L,8,R,12,R,12,R,10",
            "R,10,R,12,R,10",
            "L,10,R,10,L,6",
            "n"
            ]


--     let inp' = replaceIn inp a "A" 
--     let inp'' = replaceIn inp' b "B" 
--     let inp''' = replaceIn inp'' c "C" 
--     -- guard (all (`elem` "ABC") inp''')
--     pure (a,b,c, inp''')
--   where
--      replaceIn :: String -> String -> String -> String
--      replaceIn text pat with = text & packed . regexing (compile (B.pack pat) []) . match .~ (T.pack with)

-- data SearchState = S { seen :: (S.Set Pos), rot ::  Pos, pos ::  Pos }
--   deriving (Ord, Eq, Show)
-- searchPath = aStar step cost heuristic isDone s0
--   where
--     heuristic s = goal - (S.size (seen s))
--     isDone s = S.size (seen s) >= 200
--     cost s1 s2 = if rot s1 /= rot s2 then 2 else 1
-- step = sequence [move, turnRight,  turnLeft] `pruning` isValid
--   where
--     isValid s = M.findWithDefault '.' (pos s) mapData == '.'
--     turnLeft s = s { rot = left (rot s) }
--     turnRight s = s { rot = right (rot s) }
--     move s = s { pos = pos', seen = S.insert pos' (seen s) }
--       where pos' = plusPoint (rot s) (pos s)
-- goal = M.size $ M.filter (/='.') mapData
-- s0 = S (S.singleton p0) (0,-1) p0
--  where p0 = (8,22)



countNodes :: M.Map (Int, Int) Char -> Int
countNodes b = sum[x*y| (p@(x,y), c) <- M.toList b, (c /= '.'), countNeighbors b p > 2]
countNeighbors :: M.Map (Int, Int) Char -> (Int, Int) -> Int
countNeighbors b p = length [() | p' <- adjacent p, Just c <- pure (b M.!? p'), c /= '.']
mapData :: M.Map (Int, Int) Char
mapData = indexGrid
   ["............abbbbbbbbbbbb......................................",
    "............a...........b......................................",
    "............a.cbbbbbbbbbbbb....................................",
    "............a.c.........b.c....................................",
    "............a.c.......ddddddd..................................",
    "............a.c.........b.c.c..................................",
    "............a.c.........b.c.c.......................dcccccccccb",
    "............a.c.........b.c.c.......................d.........b",
    "..bcccccccccc.c.........b.c.c.......................d.........b",
    "..b...........c.........b.c.c.......................d.........b",
    "bbbbbbbbbbbbb.c.........b.c.c.......................d.........b",
    "b.b.........c.c.........b.c.c.......................d.........b",
    "b.b.........c.ccccccccccb.ceceeeeeeee.....ccccccccccd.........b",
    "b.b.........c...............c.......c.....e...................b",
    "b.b.........c...............ceeeeeeeeee.bcccccccccc...........b",
    "b.b.........c.......................c.c.b.e.......c...........b",
    "b.b.........c.......................c.c.b.e.......c...........b",
    "b.b.........c.......................c.c.b.e.......c...........b",
    "b.b.........c.......................c.c.b.e.......c.ccccccccccb",
    "b.b.........c.......................c.c.b.e.......c.a..........",
    "b.ccccccccccc.......................c.c.b.e.......c.a..........",
    "b...................................c.c.b.e.......c.a..........",
    "aaaaaaaa^...........................ddcdbde.......c.a..........",
    "......................................c.b.........c.a..........",
    "......................................bbbbbbbbbbbbc.a..........",
    "........................................b...........a..........",
    "........................................bbbbbbbbbbbba.........."]
type Pos = (Int, Int)
    
-- abbccbcabbccbcecdecdcbcabbccbcecd

-- L8: a
-- R12: b
-- R10: c
-- L6: d
-- L10: e

-- F: ecd
-- G: abbcc


-- 6
-- 10
-- R
-- y
-- countNodes :: M.Map (Int, Int) Char -> Int

code :: [Int]
code = [ 1,330,331,332,109,3170,1102,1182,1,16,1101,0,1469,24,102,1,0,570,1006,570,36,1002,571,1,0,1001,570,-1,570,1001,24,1,24,1106,0,18,1008,571,0,571,1001,16,1,16,1008,16,1469,570,1006,570,14,21102,1,58,0,1105,1,786,1006,332,62,99,21102,1,333,1,21101,0,73,0,1105,1,579,1101,0,0,572,1102,1,0,573,3,574,101,1,573,573,1007,574,65,570,1005,570,151,107,67,574,570,1005,570,151,1001,574,-64,574,1002,574,-1,574,1001,572,1,572,1007,572,11,570,1006,570,165,101,1182,572,127,1002,574,1,0,3,574,101,1,573,573,1008,574,10,570,1005,570,189,1008,574,44,570,1006,570,158,1106,0,81,21101,340,0,1,1106,0,177,21101,0,477,1,1106,0,177,21102,1,514,1,21102,1,176,0,1106,0,579,99,21101,0,184,0,1106,0,579,4,574,104,10,99,1007,573,22,570,1006,570,165,101,0,572,1182,21102,1,375,1,21101,0,211,0,1106,0,579,21101,1182,11,1,21102,1,222,0,1106,0,979,21102,1,388,1,21101,233,0,0,1105,1,579,21101,1182,22,1,21101,244,0,0,1105,1,979,21101,0,401,1,21102,1,255,0,1106,0,579,21101,1182,33,1,21101,0,266,0,1106,0,979,21102,414,1,1,21101,277,0,0,1106,0,579,3,575,1008,575,89,570,1008,575,121,575,1,575,570,575,3,574,1008,574,10,570,1006,570,291,104,10,21101,1182,0,1,21101,0,313,0,1105,1,622,1005,575,327,1102,1,1,575,21101,0,327,0,1105,1,786,4,438,99,0,1,1,6,77,97,105,110,58,10,33,10,69,120,112,101,99,116,101,100,32,102,117,110,99,116,105,111,110,32,110,97,109,101,32,98,117,116,32,103,111,116,58,32,0,12,70,117,110,99,116,105,111,110,32,65,58,10,12,70,117,110,99,116,105,111,110,32,66,58,10,12,70,117,110,99,116,105,111,110,32,67,58,10,23,67,111,110,116,105,110,117,111,117,115,32,118,105,100,101,111,32,102,101,101,100,63,10,0,37,10,69,120,112,101,99,116,101,100,32,82,44,32,76,44,32,111,114,32,100,105,115,116,97,110,99,101,32,98,117,116,32,103,111,116,58,32,36,10,69,120,112,101,99,116,101,100,32,99,111,109,109,97,32,111,114,32,110,101,119,108,105,110,101,32,98,117,116,32,103,111,116,58,32,43,10,68,101,102,105,110,105,116,105,111,110,115,32,109,97,121,32,98,101,32,97,116,32,109,111,115,116,32,50,48,32,99,104,97,114,97,99,116,101,114,115,33,10,94,62,118,60,0,1,0,-1,-1,0,1,0,0,0,0,0,0,1,8,22,0,109,4,1202,-3,1,587,20102,1,0,-1,22101,1,-3,-3,21101,0,0,-2,2208,-2,-1,570,1005,570,617,2201,-3,-2,609,4,0,21201,-2,1,-2,1105,1,597,109,-4,2106,0,0,109,5,1202,-4,1,630,20102,1,0,-2,22101,1,-4,-4,21101,0,0,-3,2208,-3,-2,570,1005,570,781,2201,-4,-3,653,20101,0,0,-1,1208,-1,-4,570,1005,570,709,1208,-1,-5,570,1005,570,734,1207,-1,0,570,1005,570,759,1206,-1,774,1001,578,562,684,1,0,576,576,1001,578,566,692,1,0,577,577,21101,0,702,0,1106,0,786,21201,-1,-1,-1,1105,1,676,1001,578,1,578,1008,578,4,570,1006,570,724,1001,578,-4,578,21102,1,731,0,1106,0,786,1106,0,774,1001,578,-1,578,1008,578,-1,570,1006,570,749,1001,578,4,578,21102,756,1,0,1106,0,786,1106,0,774,21202,-1,-11,1,22101,1182,1,1,21102,1,774,0,1106,0,622,21201,-3,1,-3,1105,1,640,109,-5,2106,0,0,109,7,1005,575,802,21001,576,0,-6,20101,0,577,-5,1106,0,814,21102,1,0,-1,21102,1,0,-5,21101,0,0,-6,20208,-6,576,-2,208,-5,577,570,22002,570,-2,-2,21202,-5,63,-3,22201,-6,-3,-3,22101,1469,-3,-3,1202,-3,1,843,1005,0,863,21202,-2,42,-4,22101,46,-4,-4,1206,-2,924,21101,0,1,-1,1105,1,924,1205,-2,873,21102,35,1,-4,1105,1,924,2102,1,-3,878,1008,0,1,570,1006,570,916,1001,374,1,374,2101,0,-3,895,1102,2,1,0,1202,-3,1,902,1001,438,0,438,2202,-6,-5,570,1,570,374,570,1,570,438,438,1001,578,558,921,21002,0,1,-4,1006,575,959,204,-4,22101,1,-6,-6,1208,-6,63,570,1006,570,814,104,10,22101,1,-5,-5,1208,-5,27,570,1006,570,810,104,10,1206,-1,974,99,1206,-1,974,1101,0,1,575,21101,0,973,0,1106,0,786,99,109,-7,2106,0,0,109,6,21101,0,0,-4,21101,0,0,-3,203,-2,22101,1,-3,-3,21208,-2,82,-1,1205,-1,1030,21208,-2,76,-1,1205,-1,1037,21207,-2,48,-1,1205,-1,1124,22107,57,-2,-1,1205,-1,1124,21201,-2,-48,-2,1106,0,1041,21101,-4,0,-2,1106,0,1041,21101,-5,0,-2,21201,-4,1,-4,21207,-4,11,-1,1206,-1,1138,2201,-5,-4,1059,2102,1,-2,0,203,-2,22101,1,-3,-3,21207,-2,48,-1,1205,-1,1107,22107,57,-2,-1,1205,-1,1107,21201,-2,-48,-2,2201,-5,-4,1090,20102,10,0,-1,22201,-2,-1,-2,2201,-5,-4,1103,2101,0,-2,0,1105,1,1060,21208,-2,10,-1,1205,-1,1162,21208,-2,44,-1,1206,-1,1131,1106,0,989,21102,1,439,1,1106,0,1150,21101,0,477,1,1106,0,1150,21102,1,514,1,21101,1149,0,0,1106,0,579,99,21102,1,1157,0,1105,1,579,204,-2,104,10,99,21207,-3,22,-1,1206,-1,1138,1201,-5,0,1176,1202,-4,1,0,109,-6,2106,0,0,12,13,50,1,11,1,50,1,1,13,48,1,1,1,9,1,1,1,48,1,1,1,7,7,46,1,1,1,9,1,1,1,1,1,46,1,1,1,9,1,1,1,1,1,23,11,12,1,1,1,9,1,1,1,1,1,23,1,9,1,2,11,1,1,9,1,1,1,1,1,23,1,9,1,2,1,11,1,9,1,1,1,1,1,23,1,9,14,1,1,9,1,1,1,1,1,23,1,9,2,1,1,9,1,1,1,9,1,1,1,1,1,23,1,9,2,1,1,9,1,1,11,1,11,5,11,9,2,1,1,9,1,15,1,7,1,5,1,19,2,1,1,9,1,15,11,1,11,11,2,1,1,9,1,23,1,1,1,1,1,1,1,7,1,11,2,1,1,9,1,23,1,1,1,1,1,1,1,7,1,11,2,1,1,9,1,23,1,1,1,1,1,1,1,7,1,11,2,1,1,9,1,23,1,1,1,1,1,1,1,7,1,1,12,1,1,9,1,23,1,1,1,1,1,1,1,7,1,1,1,10,1,1,11,23,1,1,1,1,1,1,1,7,1,1,1,10,1,35,1,1,1,1,1,1,1,7,1,1,1,10,9,27,7,7,1,1,1,48,1,1,1,9,1,1,1,48,13,1,1,50,1,11,1,50,13,10]
