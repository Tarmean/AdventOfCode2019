{-# Language LambdaCase #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}
module Aoc19_15 where
import IntCode
import Algorithm.Search (dijkstra, pruning)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.List (minimumBy)
import Data.Ord
import Control.Monad.Trans.Maybe
import Logic
import Control.Lens
import Control.Applicative

data Tile = Open | Wall | Goal
  deriving (Eq, Ord, Show)
type Pos = (Int, Int)
data S = S { _position :: Pos, _board :: (M.Map Pos Tile) }
makeLenses ''S

main = do
  let 
    s0 = (S (0,0) (M.singleton (0,0) Open))
    (S _ b) = execState (runMaybeT $ loop $ sealConduitT $ runMachine program inp) s0
    Just (cost, path) = search (== Just Goal) (S (0,0) b)
  print cost
  let goalPos = last path
      bfs0 = O b S.empty (S.singleton goalPos)
  print $ pred $ length $ takeWhile (not . S.null . oFrontier) $ iterate bfsStep bfs0

loop2 r pos = fromList [1..4] >>- \dir -> do
        let pos' = pos `plus` toStep dir
        unique pos'
        (o,r') <- awaitThis =<< yieldThis dir r
        case o of
          2 -> board . at pos' ?= Wall
          1 -> board . at pos' ?= Goal
          0 -> do
            board . at pos' ?= Open
            loop2 r' pos'


loop :: Monad m => Mac (StateT S m) a -> MaybeT (StateT S m) ()
loop r = do
  (_, (to:_)) <- MaybeT $ gets (search (==Nothing))
  S from b <- get
  let dir = toDirection from to
  (a,r) <- awaitThis =<< yieldThis dir r
  let
    tile' = case a of
      0 -> Wall
      1 -> Open
      2 -> Goal
    pos'
      | tile' == Wall = from 
      | otherwise = to
  put (S pos' (M.insert to tile' b))
  loop r

bfsStep :: O -> O
bfsStep (O m seen frontier) = O m seen' (frontier' `S.difference` seen')
  where
    seen' = seen `S.union` frontier
    frontier' = S.fromList $ concatMap (adjacent `pruning` notWall) $ S.toList frontier
    notWall c = (m M.! c) /= Wall
data O = O { oBoard :: (M.Map Pos Tile), oSeen :: (S.Set Pos), oFrontier :: (S.Set Pos) }


search :: (Maybe Tile -> Bool) -> S -> Maybe (Int, [Pos])
search p (S p0 m) = dijkstra neighbors (\_ _ -> 1) (p . (m M.!?)) p0
  where
    neighbors = adjacent `pruning` ((== Just Wall) . (m M.!?))
adjacent (px, py) = [(px+dx, py+dy) | dx <- [-1..1], dy <- [-1..1], abs dx + abs dy == 1]

toDirection :: Pos -> Pos -> Int
toDirection from to = head $ filter ((==to) . plus from . toStep) [1..4]
plus (a,b) (x,y) = (a+x,b+y)
toStep 1 = (0,1)
toStep 3 = (-1,0)
toStep 4 = (1,0)
toStep 2 = (0,-1)

showB t b = mapM_ putStrLn [[ out (x,y) | x <- [-25..20]] | y <- [-20..30]]
  where
    (O _ s _) = iterate bfsStep (O b S.empty (S.singleton goal)) !! t
    goal = fst . head . filter ((==Goal) . snd) $ M.toList b
    out p
      | S.member p s = '='
      | otherwise = case M.findWithDefault Open p b of
        Open -> ' '
        Open -> ' '
        Wall -> '#'
        Goal -> '*'

inp :: [Int]
inp = [3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,1002,1034,1,1039,1001,1036,0,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1105,1,124,1001,1034,0,1039,102,1,1036,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1105,1,124,1001,1034,-1,1039,1008,1036,0,1041,101,0,1035,1040,102,1,1038,1043,1001,1037,0,1042,1106,0,124,1001,1034,1,1039,1008,1036,0,1041,1001,1035,0,1040,102,1,1038,1043,1001,1037,0,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,9,1032,1006,1032,165,1008,1040,5,1032,1006,1032,165,1101,0,2,1044,1105,1,224,2,1041,1043,1032,1006,1032,179,1102,1,1,1044,1106,0,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,40,1044,1106,0,224,1101,0,0,1044,1106,0,224,1006,1044,247,102,1,1039,1034,101,0,1040,1035,101,0,1041,1036,1001,1043,0,1038,1001,1042,0,1037,4,1044,1106,0,0,26,29,83,66,1,36,14,44,33,12,3,15,20,56,9,35,51,55,6,20,13,71,15,23,94,38,45,15,47,30,89,39,11,55,5,9,47,29,41,36,78,12,4,65,48,66,36,94,76,30,63,41,32,1,73,1,35,65,87,46,18,90,11,44,30,73,87,8,38,46,17,78,51,34,19,53,37,26,20,24,46,64,17,6,26,41,10,62,14,88,23,94,13,55,5,45,10,39,83,99,32,34,72,30,58,33,71,47,21,38,97,38,46,41,18,39,37,8,86,55,35,4,92,19,21,53,61,6,55,69,16,85,62,26,63,17,80,33,10,53,91,2,37,94,37,93,7,97,18,55,54,36,17,62,89,12,92,32,69,4,46,47,19,89,25,12,51,91,9,1,71,35,56,39,98,48,7,49,24,95,15,45,2,1,93,82,19,7,11,70,30,64,28,27,58,4,39,30,94,72,33,43,90,98,26,32,70,1,81,25,35,47,17,31,92,15,73,13,27,72,65,30,67,2,22,89,77,30,47,12,58,26,79,22,37,74,41,3,42,30,39,67,24,18,62,98,19,59,95,25,6,67,42,35,85,51,48,7,63,17,67,53,45,13,25,43,1,54,4,65,55,20,73,32,70,1,33,39,93,88,19,35,56,21,13,53,73,31,21,44,73,31,13,69,30,42,26,51,25,90,16,49,9,93,50,28,60,24,18,61,23,11,98,19,45,77,12,61,31,3,66,56,4,77,24,59,87,31,38,65,67,7,9,23,71,9,59,35,55,83,22,12,94,17,67,87,96,63,8,29,32,34,15,55,39,60,41,74,39,81,47,51,25,26,57,28,18,60,84,20,16,66,42,14,25,16,94,2,22,74,85,19,63,32,9,19,11,91,44,34,21,1,56,12,87,8,52,18,56,7,90,5,86,81,24,98,21,9,80,59,68,10,80,53,18,75,50,9,14,43,26,29,57,86,39,41,93,3,69,55,16,84,15,22,84,30,72,19,13,15,19,80,97,79,32,68,77,82,30,19,4,71,45,67,14,95,17,54,80,88,25,13,80,41,37,96,15,28,26,33,73,32,45,79,21,52,23,98,82,21,16,13,64,32,39,93,17,33,95,61,36,12,21,3,84,4,88,22,26,59,80,27,82,2,85,79,29,33,52,17,23,95,8,64,16,56,23,42,43,18,41,11,9,84,42,62,4,67,17,98,76,99,1,16,72,72,10,79,19,76,4,54,9,99,34,33,7,97,85,19,76,93,38,6,90,37,90,2,83,61,19,43,39,2,91,17,60,21,79,2,32,94,38,32,7,64,8,14,7,68,23,28,75,24,73,50,29,63,22,89,4,51,66,2,7,33,82,13,23,84,81,23,55,68,15,27,9,97,27,79,42,86,75,56,13,95,74,5,88,25,44,99,33,14,24,29,21,78,4,15,75,32,92,74,11,56,24,57,10,28,73,8,10,90,77,30,96,8,60,3,71,20,41,9,33,89,38,74,95,4,95,35,13,18,55,10,81,9,60,17,67,7,34,48,48,15,54,79,37,66,43,22,64,28,28,4,91,5,9,92,30,64,37,98,66,15,92,2,3,25,70,25,33,61,56,25,70,58,30,41,97,18,54,10,49,45,3,1,30,57,30,46,8,55,79,39,58,46,35,19,38,80,86,4,36,75,29,62,39,71,2,41,6,66,36,99,21,61,39,72,3,48,29,43,31,59,84,71,12,52,61,82,11,56,23,51,30,60,88,65,35,48,24,58,76,49,93,51,33,72,0,0,21,21,1,10,1,0,0,0,0,0,0]
