{-#Language LambdaCase #-}
{-#Language FlexibleContexts #-}
{-#Language TypeFamilies #-}
{-#Language ExistentialQuantification #-}
{-#Language RankNTypes #-}
{-#Language FlexibleInstances, MultiParamTypeClasses #-}
{-#Language UnboxedTuples, DataKinds, PolyKinds, UndecidableInstances #-}
{-#Language GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Aoc19_5 where
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Unboxed as V
import Control.Applicative
import Control.Monad.State
import Control.Monad.ST
import GHC.Magic

main :: IO ()
main = do
  let go ins = runMachine (runStateT program (ins, [])) ls 
  print $ go [1]


program :: StateT ([Int], [Int]) (M b) ()
program = cut go
  where go = execOp >> go

{-# INLINE execOp #-}
execOp :: (Machine m, MachineIO m) => m ()
execOp = do
    (vals, i) <- pOp =<< readInstruction
    case i of
      99 -> halt
      3 -> do
         let [j] = vals
         m <- input
         writeAt j m
      4 -> do
         let [o] = vals
         output o
      1 -> binOp (+) vals
      2 -> binOp (*) vals
      _ -> error ("unkown op" <> show i)
  where
    binOp p = \[l, r, t] -> writeAt t (p l r)

pOp :: Machine m => Int -> m ([Int], Int)
pOp = go id
  where
   go acc i
     | i < 100 = pure (acc (replicate 0), i)
     | otherwise = do
         let (l,r) = divMod i 100
             mode = case l of
               0 -> Val
               1 -> Lit
               _ -> error ("unknown mode")
         s <- readInstruction
         v <- evalVal (V s mode)
         go (acc . (v :)) r
{-# INLINE readInstruction #-}
readInstruction :: Machine m => m Offset
readInstruction = do
  i <- getIP
  setIP (i+1)
  readAt i

evalVal :: Machine m => Value -> m Int
evalVal (V i Lit) = pure i
evalVal (V i Val) = readAt i

runMachine :: (forall b. M b a) -> V.Vector Int -> (V.Vector Int, Maybe a)
runMachine m v = runST $ do
    vm <- V.thaw v
    ma <- runM m vm  0 (\_i a -> pure (Just a)) (pure Nothing)
    v' <- V.unsafeFreeze vm
    pure (v', ma)

type Offset = Int
data Mode = Val | Lit
  deriving Show
data Op = BinOp BOP Value Value Offset |  Done | Jump Offset
  deriving Show
data BOP = Plus | Mult
  deriving Show
data Value = V !Int Mode
  deriving Show
newtype M s a = M { runM :: forall b. U.MVector s Int -> Int -> (Int -> a -> ST s b) -> ST s b -> ST s b }
  deriving Functor
instance Applicative (M b) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  M hf <*> M ha = M $ \v i c e -> hf v i (\i' f -> ha v  i' (\i'' a -> c i'' (f a)) e) e

instance Alternative (M b) where
  empty = halt
  M a <|> M b = M $ \v i c e -> a v i c (b v i c e)
instance MonadPlus (M b) where
  mzero = empty
  mplus = (<|>) 
{-# INLINE cut #-}
cut :: (Alternative m) => m a -> m ()
cut a = go <|> pure ()
  where go = a *> go

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
    halt :: m a
class (Monad m) => MachineIO m where
    input :: m Int
    output :: Int -> m ()
instance Monad m => MachineIO (StateT ([Int], [Int]) m) where
    input = do
        (l, y) <- get
        let (x:xs) = l
        put (xs,y)
        pure x
    output v = do
        (x,y) <- get
        put (x,v:y)
instance Machine m => Machine (StateT s m) where
    readAt = lift . readAt
    writeAt a b = lift (writeAt a b)
    getIP = lift getIP
    modifyIP = lift .modifyIP
    setIP = lift .setIP
    halt = lift halt
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
ls = V.fromList [3,225,1,225,6,6,1100,1,238,225,104,0,1001,191,50,224,101,-64,224,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,2,150,218,224,1001,224,-1537,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1002,154,5,224,101,-35,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1102,76,17,225,1102,21,44,224,1001,224,-924,224,4,224,102,8,223,223,1001,224,4,224,1,224,223,223,101,37,161,224,101,-70,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,102,46,157,224,1001,224,-1978,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,5,29,225,1101,10,7,225,1101,43,38,225,1102,33,46,225,1,80,188,224,1001,224,-73,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1101,52,56,225,1101,14,22,225,1101,66,49,224,1001,224,-115,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1101,25,53,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,226,226,224,1002,223,2,223,1005,224,329,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,344,1001,223,1,223,8,677,677,224,102,2,223,223,1006,224,359,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,374,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,389,101,1,223,223,7,677,226,224,1002,223,2,223,1006,224,404,1001,223,1,223,1107,677,226,224,1002,223,2,223,1006,224,419,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,434,101,1,223,223,1008,226,677,224,102,2,223,223,1005,224,449,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,479,101,1,223,223,1007,226,677,224,1002,223,2,223,1005,224,494,1001,223,1,223,108,226,677,224,1002,223,2,223,1006,224,509,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,524,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,539,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,554,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,584,1001,223,1,223,1008,677,677,224,102,2,223,223,1005,224,599,1001,223,1,223,1107,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,7,226,226,224,102,2,223,223,1005,224,629,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,644,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226]
