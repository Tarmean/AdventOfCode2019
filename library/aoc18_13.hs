{-# OPTIONS_GHC -fdefer-type-errors #-}
module Aoc18_13 where
-- import qualified Data.Massiv as M
import qualified Data.Map as M
import Control.Monad

data Dir = Up | DRight | Down | DLeft | Crash
  deriving (Eq, Show, Enum)

data Slot = Empty | Hor | Vert | Slash | Backslash | Crossing
  deriving (Eq, Show, Enum)
data CartState = CartState { phase :: CartPhase, dir :: Dir }
  deriving (Eq, Show)
data CartPhase = R | S | L
  deriving (Eq, Show, Enum)

main = do
    s <- readFile "input"
    let 
      p = parseField s
      carts = toCarts p
      slots = toSlots p
    print  (loop slots carts)
type Pos = (Int, Int)
type Carts = M.Map Pos Dir

parseField :: String -> [[(Slot, Maybe Dir)]]
parseField s = map (map parseSlot) $ lines s
toCarts :: [[(Slot, Maybe Dir)]] -> M.Map (Int, Int) CartState
toCarts lls
  = M.fromList
  [((j,i) , CartState L d)
  | (i, ls) <- zip [0..] lls
  , (j, (_, Just d)) <- zip [0..] ls
  ]
toSlots :: [[(Slot, Maybe Dir)]] -> M.Map (Int, Int) Slot
toSlots lls
   = M.fromList
   [((j,i) , s)
   | (i, ls) <- zip [0..] lls
   , (j, (s, _)) <- zip [0..] ls
   ]
 
parseSlot :: Char -> (Slot, Maybe Dir)
parseSlot ' ' = (Empty, Nothing)
parseSlot '-' = (Hor, Nothing)
parseSlot '|' = (Vert, Nothing)
parseSlot 'v' = (Vert, Just Down)
parseSlot '>' = (Hor, Just DRight)
parseSlot '<' = (Hor, Just DLeft)
parseSlot '^' = (Vert, Just Up)
parseSlot '/' = (Slash, Nothing)
parseSlot '\\' = (Backslash, Nothing)
parseSlot '+' = (Crossing, Nothing)
parseSlot l = error( "'" <> show l <> "'")

stepDir i dir = toEnum $ (fromEnum dir - i) `mod` 4
stepPhase :: CartPhase -> CartPhase
stepPhase r = toEnum ((fromEnum r - 1) `mod` 3) 

fromPhase L = (-1)
fromPhase R = (1)
fromPhase S = 0
step :: Slot -> CartState -> CartState
step Slash r = r { dir =  stepSlash (dir r) }
  where
    stepSlash DRight = Up
    stepSlash Up = DRight
    stepSlash DLeft = Down
    stepSlash Down = DLeft
step Backslash r =  r { dir = stepSlash (dir r) }
  where
    stepSlash DLeft = Up
    stepSlash Up = DLeft
    stepSlash DRight = Down
    stepSlash Down = DRight
step Crossing r = CartState { phase = stepPhase (phase r), dir = stepDir i (dir r) }
  where i = (fromEnum (phase r) - 1)
step _ r = r
addDir :: Dir -> (Int, Int) -> (Int, Int)
addDir Up (x,y) = (x,y-1)
addDir Down (x,y) = (x,y+1)
addDir DLeft (x,y) = (x-1,y)
addDir DRight (x,y) = (x+1,y)

loop m n0 = go n0
  where
     go n
       | M.size n' <= 1 = M.toList n'
       | otherwise = go n'
       where n' = M.filter ((/= Crash) . dir) $ foldCalc m n

foldCalc :: M.Map (Int, Int) Slot -> M.Map (Int, Int) CartState -> (M.Map (Int, Int) CartState )
foldCalc m n = foldl step (n) (M.toAscList n)
  where
    step (n') (curPos, curState)
      | (n' M.!? curPos) /= Just curState = n'
      | not (M.member  newPos n') = M.insert newPos newState (M.delete curPos n')
      |otherwise  = M.insert newPos (curState {dir = Crash }) (M.delete curPos n')
      where (newPos, newState) = calc m curPos curState
calc :: M.Map (Int, Int) Slot -> (Int, Int)-> CartState ->  ((Int, Int), CartState)
calc m pos s = (addDir (dir s') pos, s')
  where
   s' =step cell s
   cell = M.findWithDefault Empty  pos m
