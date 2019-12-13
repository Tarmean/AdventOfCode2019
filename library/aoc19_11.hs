{-# Language DeriveFunctor #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances, MultiParamTypeClasses #-}
{-# Language TypeFamilies #-}
{-# Language LambdaCase #-}
{-# Language UndecidableInstances #-}
module Aoc19_11 where
import IntCode
import qualified Data.Vector.Unboxed as V
import Control.Monad.Trans
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Control.Lens
import qualified Control.Monad.Fail as F
import Control.Monad.Primitive

step ::  S a -> M (S a)
step r = do
    c <- getColor
    r <- putV c r
    (a,r) <- getV r
    setColor a
    (b,r) <- getV r
    let
      b' = case b of
               0 -> -1 
               1 -> 1
    d <- _2 <%= addDir b'
    _1 %= move d
    pure r

main = do
  ((), (_, _, m)) <-  runStateT run ((0,0), DUp, M.empty)
  print $ length  $ M.elems m
  ((), (_, _, m)) <-  runStateT run ((0,0), DUp, M.singleton (0,0) Black)
  mapM_ putStrLn (toS m)

run = loop (mkRobot ls)
loop r = (step r >>= loop) `mplus` pure ()

data Trampoline m a = In (Int -> Trampoline m a) | Out Int (Trampoline m a) | Done a | Eff (m (Trampoline m a))
  deriving Functor

getColor :: M Int
getColor = do
    pos <- use _1
    b <- preuse (_3 . ix pos)
    let c = case b of
          Just Black -> 1
          _ -> 0
    pure c
setColor :: Int -> M ()
setColor c = do
    pos <- use _1
    let b = case c of
          1 -> Black
          0 -> White
    _3 . at pos .= Just b

move DUp (x,y) = (x,y+1)
move DDown (x,y) = (x,y-1)
move DLeft (x,y) = (x-1,y)
move DRight (x,y) = (x+1,y)
          
toS :: M.Map Point Color -> [String]
toS m = do [[if m M.!? (x,-y) == Just Black then '█' else ' ' | x <- [0..40]] | y <- [-2..8]]
    
instance PrimMonad m => PrimMonad (Trampoline m) where
  type instance PrimState (Trampoline m) = PrimState m
  primitive = lift . primitive
instance F.MonadFail m => F.MonadFail (Trampoline m) where
  fail = lift . F.fail
instance Monad m => Applicative (Trampoline m) where
    (<*>) = ap
    pure = return
instance Monad m => Monad (Trampoline m) where
    return = Done
    Done r >>= f = f r
    In l >>= f = In (\a -> l a >>= f)
    Out i l >>= f = Out i (l >>= f)
    Eff m >>= f = Eff ((>>=f) <$> m)

instance MonadState s m => MonadState s (Trampoline m) where
    get = lift get
    put = lift . put
instance MonadTrans Trampoline where
    lift = Eff . fmap Done
instance Machine s m => Machine s (Trampoline m) where
    halt = lift halt
instance Monad m => MachineIO (Trampoline m) where
    input = In Done
    output i = Out i (Done ())


data Color = Black | White
  deriving (Show, Eq, Ord)
data Dir = DUp|DRight|DDown|DLeft
  deriving (Bounded, Enum, Show)
addDir :: Int -> Dir -> Dir
addDir i d = toEnum ((i + fromEnum d) `mod` size)
  where size = fromEnum (maxBound::Dir) - fromEnum (minBound::Dir) + 1
type Point = (Int, Int)
type S a = Trampoline (StateT (Point, Dir, M.Map Point Color) IO) a
type M a = StateT (Point, Dir, M.Map Point Color) IO a
mkRobot :: V.Vector Int -> S ()
mkRobot = fmap snd (runMachine program)

getV :: MonadPlus m => Trampoline m a -> m (Int, Trampoline m a)
getV (Done _) = mzero
getV (Eff a) = a >>= getV
getV (Out i a) = pure (i, a)
getV (In _) = error "shouldn't be in"

putV :: (MonadPlus m) => Int -> Trampoline m a -> m (Trampoline m a)
putV _ (Done _) = mzero
putV i (Eff a) = a >>= putV i
putV _ (Out _ _) = error "shouldn't be out"
putV i (In f) = pure (f i)
ls :: V.Vector Int
ls = V.fromList [ 3,8,1005,8,328,1106,0,11,0,0,0,104,1,104,0,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,29,1,104,7,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1001,8,0,55,1,2,7,10,1006,0,23,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,84,1006,0,40,1,1103,14,10,1,1006,16,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,1002,8,1,116,1006,0,53,1,1104,16,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,102,1,8,146,2,1104,9,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,172,1006,0,65,1,1005,8,10,1,1002,16,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,204,2,1104,9,10,1006,0,30,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,233,2,1109,6,10,1006,0,17,1,2,6,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,266,1,106,7,10,2,109,2,10,2,9,8,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,301,1,109,9,10,1006,0,14,101,1,9,9,1007,9,1083,10,1005,10,15,99,109,650,104,0,104,1,21102,1,837548789788,1,21101,0,345,0,1106,0,449,21101,0,846801511180,1,21101,0,356,0,1106,0,449,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,235244981271,0,1,21101,403,0,0,1105,1,449,21102,1,206182744295,1,21101,0,414,0,1105,1,449,3,10,104,0,104,0,3,10,104,0,104,0,21102,837896937832,1,1,21101,0,437,0,1106,0,449,21101,867965862668,0,1,21102,448,1,0,1106,0,449,99,109,2,22102,1,-1,1,21101,40,0,2,21102,1,480,3,21101,0,470,0,1106,0,513,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,475,476,491,4,0,1001,475,1,475,108,4,475,10,1006,10,507,1101,0,0,475,109,-2,2106,0,0,0,109,4,1201,-1,0,512,1207,-3,0,10,1006,10,530,21102,1,0,-3,22102,1,-3,1,21201,-2,0,2,21102,1,1,3,21102,549,1,0,1106,0,554,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,577,2207,-4,-2,10,1006,10,577,21202,-4,1,-4,1106,0,645,21202,-4,1,1,21201,-3,-1,2,21202,-2,2,3,21101,596,0,0,1106,0,554,21201,1,0,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,615,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,637,22102,1,-1,1,21101,637,0,0,105,1,512,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0 ] <> V.replicate 4096 0
