{-#Language RankNTypes #-}
{-#Language GeneralizedNewtypeDeriving, DeriveFunctor #-}
module IntCode (program, execOp, MachineIO(..), Machine(..), runMachine) where
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Unboxed as V
import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Fail as F
import Control.Monad.Primitive
import GHC.Magic

program :: (MonadFail m, Machine m, MachineIO m, Alternative m) => m ()
program = loop execOp

{-# INLINE execOp #-}
execOp :: (MonadFail m, Machine m, MachineIO m) => m ()
execOp = do
    (i, a:b:_) <- pTags <$> readInstruction
    let
      binOp p = store =<< liftA2 p (load a) (load b)
      tagged p = \x y -> if p x y then 1 else 0
      jumpIf p = do
        l <- load a
        t <- load b
        when (p l) (setIP t)
    case i of
      99 -> halt
      1 -> binOp (+)
      2 -> binOp (*)
      7 -> binOp $ tagged (<)
      8 -> binOp $ tagged (==)
      5 -> jumpIf (/=0)
      6 -> jumpIf (== 0)
      4 -> output =<< load a
      3 -> store =<< input
      _ -> error ("unkown op" <> show i)
store :: Machine m => Int -> m ()
store v = do
    t <- readInstruction
    writeAt t v

pTags :: Int -> (Int, [Int])
pTags a = (m0, go d0)
  where
   (d0, m0) = divMod a 100
   go i = let (d,m) = divMod i 10 in m : go d
{-# INLINE readInstruction #-}
readInstruction :: Machine m => m Offset
readInstruction = do
  i <- getIP
  setIP (i+1)
  readAt i

load :: Machine m => Int -> m Int
load 0 = readAt =<< readInstruction
load 1 = readInstruction
load a = error ("Unknown tag" <> show a)

runMachine :: (PrimMonad m) => M m a -> V.Vector Int -> m (V.Vector Int, Maybe a)
runMachine m v = do
    vm <- V.thaw v
    ma <- runM m vm  0 (\_i a -> pure (Just a)) (pure Nothing)
    v' <- V.unsafeFreeze vm
    pure (v', ma)

type Offset = Int
newtype M m a = M { runM :: forall b. U.MVector (PrimState m) Int -> Int -> (Int -> a -> m b) -> m b -> m b }
  deriving Functor
instance Applicative (M b) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  M hf <*> M ha = M $ \v i c e -> hf v i (\i' f -> ha v  i' (\i'' a -> c i'' (f a)) e) e

instance PrimMonad b => Alternative (M b) where
  empty = halt
  M a <|> M b = M $ \v i c e -> a v i c (b v i c e)
instance PrimMonad b => MonadPlus (M b) where
  mzero = empty
  mplus = (<|>) 
{-# INLINE loop #-}
loop :: (Alternative m) => m a -> m ()
loop a = go <|> pure ()
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
instance MonadTrans M where
    lift m = M $ \_v i c _e -> c i =<< m
instance MachineIO m => MachineIO (M m) where
    input = lift input
    output = lift . output
instance MachineIO IO where
    input = readIO =<< getLine
    output = print
instance MonadFail m => MonadFail (M m) where
   fail = lift . F.fail
instance PrimMonad m => Machine (M m) where
  {-# INLINE readAt #-}
  {-# INLINE writeAt #-}
  {-# INLINE getIP #-}
  {-# INLINE setIP #-}
  -- elide bounds checks for 4x speedup and 5x allocation reduction
  -- this would result in UB if the input is malformed or there is a logic bug,
  -- though.
  -- Since the boundschecks really should stay enabled further optimizations don't really feel worthwhile
  readAt o = M $ \v  i c _e -> c i =<< (U.read v o)
  writeAt o j = M $ \v  i c _e -> c i =<< U.write v o j
  getIP = M $ \_v  i c _e -> c i i
  modifyIP f = M $ \_v  i c _e -> c (f i) ()
  setIP o =  modifyIP (const o)
  halt = M $ \_ _ _ e -> e

