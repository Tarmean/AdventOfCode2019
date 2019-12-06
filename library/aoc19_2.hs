{-#Language LambdaCase #-}
{-#Language FlexibleContexts #-}
{-#Language TypeFamilies #-}
{-#Language ExistentialQuantification #-}
{-#Language RankNTypes #-}
{-#Language FlexibleInstances, MultiParamTypeClasses #-}
{-#Language UnboxedTuples, DataKinds, PolyKinds, UndecidableInstances #-}
{-#Language GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Aoc19_2 where
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Unboxed as V
import Control.Applicative
import Control.Monad.ST
import GHC.Magic

main :: IO ()
main = do
  let go i j = runMachine (writeAt 1 i >> writeAt 2 j >> program) ls
      first = V.head . fst
  print $ first $ go 12 2
  print $ head [i * 100 + j | i <- [0..99], j <- [0..99], first (go i j) == 19690720]

{-# INLINE step #-}
step :: Machine m => m ()
step = execOp =<< parseOp

program :: M b ()
program = () <$ cut go
  where go = step >> go

{-# INLINE parseOp #-}
parseOp :: (Machine m) => m Op
parseOp = do
    i <- readInstruction
    case i of
      99 -> pure Done
      1 -> binOp Plus
      2 -> binOp Mult
      _ -> undefined
  where
    binOp p = BinOp p <$> readInstruction <*> readInstruction <*> readInstruction

{-# INLINE readInstruction #-}
readInstruction :: Machine m =>  m Int
readInstruction = do
  i <- getIP
  setIP (i+1)
  readAt i

{-# INLINE execOp #-}
execOp :: Machine m => Op -> m ()
execOp Done = halt
execOp (BinOp p vl vr t) = do
    l <- readAt vl
    r <- readAt vr
    let o = case p of
         Mult -> l * r
         Plus -> l + r
    writeAt t o
execOp (Jump i) = setIP i

runMachine :: (forall b. M b a) -> V.Vector Int -> (V.Vector Int, Maybe a)
runMachine m v = runST $ do
    vm <- V.thaw v
    ma <- runM m vm  0 (\_i a -> pure (Just a)) (pure Nothing)
    v' <- V.unsafeFreeze vm
    pure (v', ma)

data Op = BinOp BOP Offset Offset Offset |  Done | Jump Offset
  deriving Show
data BOP = Plus | Mult
  deriving Show
type Offset = Int
newtype M s a = M { runM :: forall b. U.MVector s Int -> Int -> (Int -> a -> ST s b) -> ST s b -> ST s b }
  deriving Functor
instance Applicative (M b) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  M hf <*> M ha = M $ \v i c e -> hf v i (\i' f -> ha v  i' (\i'' a -> c i'' (f a)) e) e

{-# INLINE cut #-}
cut :: M b a -> M b (Maybe a)
cut (M f) = M $ \v  i c _e -> f v  i (\i' a -> c i' (Just a)) (c i Nothing)

instance Monad (M b) where

  {-# INLINE return #-}
  return a = M (\_ i c _->  c i a)
  {-# INLINE (>>=) #-}
  M ha >>= f = M $ oneShot $ \v i c e -> ha v  i (oneShot $ \i' a -> runM (f a) v i' c e) e 
  

class (Monad m) => Machine m where
    readAt :: Offset -> m Int
    writeAt :: Offset -> Int -> m ()
    getIP :: m Offset
    setIP :: Offset -> m ()
    modifyIP :: (Offset -> Offset) -> m ()
    halt :: m ()
instance Machine (M s) where
  {-# INLINE readAt #-}
  {-# INLINE writeAt #-}
  {-# INLINE getIP #-}
  {-# INLINE setIP #-}
  -- elide bounds checks for 4x speedup and 5x allocation reduction
  -- this would result in UB if the input is malformed or there is a logic bug,
  -- though.
  -- Since the boundschecks really should stay enabled further optimizations don't really feel worthwhile
  readAt o = M $ \v  i c _e -> c i =<< (U.unsafeRead v o)
  writeAt o j = M $ \v  i c _e -> c i =<< U.unsafeWrite v o j
  getIP = M $ \_v  i c _e -> c i i
  modifyIP f = M $ \_v  i c _e -> c (f i) ()
  setIP o =  modifyIP (const o)
  halt = M $ \_ _ _ e -> e


ls :: V.Vector Int
ls = V.fromList [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,10,19,23,1,23,9,27,1,5,27,31,2,31,13,35,1,35,5,39,1,39,5,43,2,13,43,47,2,47,10,51,1,51,6,55,2,55,9,59,1,59,5,63,1,63,13,67,2,67,6,71,1,71,5,75,1,75,5,79,1,79,9,83,1,10,83,87,1,87,10,91,1,91,9,95,1,10,95,99,1,10,99,103,2,103,10,107,1,107,9,111,2,6,111,115,1,5,115,119,2,119,13,123,1,6,123,127,2,9,127,131,1,131,5,135,1,135,13,139,1,139,10,143,1,2,143,147,1,147,10,0,99,2,0,14,0]
