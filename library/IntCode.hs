{-#Language RankNTypes #-}
{-#Language KindSignatures #-}
{-#Language TypeFamilies #-}
{-#Language QuantifiedConstraints, ConstraintKinds #-}
{-#Language UndecidableInstances #-}
{-#Language FunctionalDependencies #-}
{-#Language GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-#Language FlexibleInstances #-}
module IntCode (program, runMachine, Machine, MachineIO(..), runS, memory, yieldThis, awaitThis, I.sealConduitT, Mac) where
import Control.Applicative
import Data.Foldable (toList)
import Control.Monad.Trans
import Control.Lens
import Control.Monad
import Control.Monad.Fail as F
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import qualified Data.IntMap as M
import Data.Conduit
import qualified Data.Conduit.Internal as I
import Data.Maybe (fromJust)

type Mac m a = I.SealedConduitT Int Int m a
instance (Monad m, l ~ Int, r ~ Int) => MachineIO (ConduitT l r m) where
    input = fmap fromJust await
    output = yield

yieldThis :: (MonadTrans n, Monad (n m), Alternative (n m), Monad m) => l -> I.SealedConduitT l r m a -> n m (I.SealedConduitT l r m a)
yieldThis _ (I.SealedConduitT (I.Done _)) = empty
yieldThis _ (I.SealedConduitT (I.HaveOutput _ _)) = empty
yieldThis i (I.SealedConduitT (I.NeedInput f _)) = pure (I.SealedConduitT (f i))
yieldThis i (I.SealedConduitT (I.PipeM msrc)) = lift msrc >>= yieldThis i . I.SealedConduitT
yieldThis i (I.SealedConduitT (I.Leftover a l)) = fmap addLeftover (yieldThis i (I.SealedConduitT a))
  where addLeftover (I.SealedConduitT r) = I.SealedConduitT (I.Leftover r l)
awaitThis :: (MonadTrans n, Monad (n m), Alternative (n m), Monad m) => I.SealedConduitT l r m a -> n m (r, I.SealedConduitT l r m a)
awaitThis (I.SealedConduitT (I.Done _)) = empty
awaitThis (I.SealedConduitT (I.NeedInput _ _)) = empty
awaitThis (I.SealedConduitT (I.HaveOutput src x)) = pure ( x,I.SealedConduitT src)
awaitThis (I.SealedConduitT (I.PipeM msrc)) = lift msrc >>= awaitThis . I.SealedConduitT
awaitThis (I.SealedConduitT (I.Leftover a l)) = fmap addLeftover (awaitThis (I.SealedConduitT a))
  where addLeftover (r,I.SealedConduitT c) = ( r,I.SealedConduitT (I.Leftover c l))

instance Monad m => MachineIO (S m) where
    input = S $ do
        ls <- get
        let (x:xs) = ls
        put xs
        pure x
    output a = S (tell [a])
newtype S m a = S (StateT [Int] (WriterT [Int] m) a)
  deriving (Functor, Monad, Applicative)
runS :: Monad m => S m a  -> [Int] -> m [Int]
runS (S m) a= execWriterT (runStateT m a)


program :: (Machine s m, MachineIO m, Alternative m) => m ()
program = loop execOp

{-# INLINE execOp #-}
execOp :: (Machine s m, MachineIO m) => m ()
execOp = do
    (i, flags) <- pTags <$> readInstruction
    let (a:b:c:_) = flags
    let
      binOp p = store c =<< liftA2 p (load a) (load b)
      tagged p = \x y -> if p x y then 1 else 0
      jumpIf p = do
        l <- load a
        t <- load b
        when (p l) (ip .= t)
    case i of
      99 -> halt
      1 -> binOp (+)
      2 -> binOp (*)
      7 -> binOp $ tagged (<)
      8 -> binOp $ tagged (==)
      5 -> jumpIf (/=0)
      6 -> jumpIf (== 0)
      4 -> output =<< load a
      3 -> store a =<< input
      9 -> (relativeBase .=) =<< liftA2 (+) (load a) (use relativeBase)
      _ -> error ("unkown op" <> show i)
store :: Machine s m => Int -> Int -> m ()
store 0 v = do
    t <- readInstruction
    memory t .= v
store 2 v = do
    t <- readInstruction
    b <- use relativeBase
    memory (b+t) .=  v
store i _ = error ("illegal store mode " <> ": " <> show i)

pTags :: Int -> (Int, [Int])
pTags a = (m0, go d0)
  where
   (d0, m0) = divMod a 100
   go i = let (d,m) = divMod i 10 in m : go d
{-# INLINE readInstruction #-}
readInstruction :: Machine s m => m Offset
readInstruction = do
  i <- use ip
  ip .= (i+1)
  use (memory i)

load :: (Machine s m) => Int -> m Int
load 0 = readInstruction >>= (use . memory)
load 1 = readInstruction
load 2 = do
    i <- readInstruction
    b <- use relativeBase
    use $ memory (b+i)
load a = error ("Unknown tag" <> show a)

runMachine :: (Foldable f, Monad m) => M m a -> f Int -> m (Maybe a, (Int, Int, M.IntMap Int))
runMachine m v = do
    let vm = M.fromList (zip [0..] (toList v))
    ma <- runStateT (runMaybeT (runM m)) (0,0,vm)  
    pure  ma

type Offset = Int
newtype M m a = M { runM :: MaybeT (StateT (Int, Int, M.IntMap Int) m) a }
  deriving (Functor, Monad, Applicative, Alternative, MonadPlus)
instance(Monad m, (Int, Int, M.IntMap Int)~s, MachineState s) => MonadState s (M m) where
   get = M get
   put = M . put
loop :: (Alternative m) => m a -> m ()
loop a = go <|> pure ()
  where go = a *> go

class MachineState s where
    memory :: Offset -> Lens' s Int
    ip :: Lens' s Offset
    relativeBase :: Lens' s Offset
instance MachineState (Int, Int, M.IntMap Int) where
  memory o = _3 . at o . non 0
  ip = _1
  relativeBase = _2
class Monad m => MonadHalt m where
    halt :: m a
class (MonadHalt m, MonadState s m, MachineState s) => Machine s m where
class (Monad m) => MachineIO m where
    input :: m Int
    output :: Int -> m ()
instance MachineIO m => MachineIO (M m) where
    input = lift input
    output = lift . output
instance MachineIO IO where
    input = readIO =<< getLine
    output = print
instance MonadTrans M where
  lift = M . lift . lift

instance MonadFail m => MonadFail (M m) where
   fail = lift . F.fail
instance Monad m => Machine (Int, Int, M.IntMap Int) (M m) where
instance Monad m => MonadHalt (M m) where
  halt = empty
-- instance (Monad m, MonadHalt m) => MonadHalt (ConduitT i o m) where
--   halt = lift halt
--   -- elide bounds checks for 4x speedup and 5x allocation reduction
  -- this would result in UB if the input is malformed or there is a logic bug,
  -- though.
  -- Since the boundschecks really should stay enabled further optimizations don't really feel worthwhile

