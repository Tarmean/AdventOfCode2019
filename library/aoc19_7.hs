{-# Language FlexibleInstances #-}
{-# Language TypeFamilies #-}
{-# Language TupleSections #-}
{-# Language FlexibleContexts #-}
module Aoc19_7 where

import Conduit
import qualified Data.Conduit.Internal as I
import qualified Data.Vector.Unboxed as V
import Data.Maybe (fromJust)
import IntCode
import Control.Monad.Fail
import Control.Monad
import Data.List (maximumBy, permutations)
import Data.Ord


main :: IO ()
main = do
   let getMax = print . maximumBy (comparing snd)
       f1 ls = done (yield 0 .| steps ls)
       f2 ls = done $ whileIncreasing (yield 0 .| loop (steps ls))
   getMax =<< candidates [0..4] f1
   getMax =<< candidates [5..9] f2

candidates :: [Int] -> ([Int] -> IO b) -> IO [([Int], b)]
candidates vs f = sequence [fmap (ls,) (f ls) | ls <- permutations vs]

steps :: (Foldable t, PrimMonad m, MonadFail m, Functor t) => t Int -> ConduitM Int Int m ()
steps ls = foldr1 (.|) (fmap step ls)
  where step x = withInit x .|  (() <$runMachine (program) v)

withInit :: MachineIO m => Int -> m ()
withInit a = output a >> forever (input >>= output)

whileIncreasing :: (Ord i, Monad m, Show i) => ConduitT i o m () -> ConduitT i o m ()
whileIncreasing m = slidingWindowC 2 .| takeWhileC p .| mapC last .| m
  where
    p [a,b] = a < b
    p _ = True

done :: Monad m => ConduitT () i m () -> m (Maybe i)
done a = runConduit (a .| getLast)
  where
    getLast = await >>= maybe (pure Nothing) go
    go a0 = await >>= maybe (pure $ Just a0) go

instance (MonadFail m, Monad m, l ~ Int, r ~ Int) => MachineIO (ConduitT l r m) where
    input = fmap fromJust await
    output = yield
instance (Machine m) => Machine (ConduitT l r m) where
    readAt = lift . readAt
    writeAt a b = lift (writeAt a b)
    getIP = lift getIP
    setIP = lift . setIP
    modifyIP = lift . modifyIP
    halt = lift halt

loop :: Monad m => ConduitT i i m a -> ConduitT i i m a
loop = unsealConduitT . go0 . sealConduitT
  where
    go0 (I.SealedConduitT a) = I.SealedConduitT (go ([],[]) a)
    go _ (I.Done a) = I.Done a
    go xs (I.HaveOutput src x) = I.HaveOutput (go (push x xs) src) x
    go xs (I.PipeM msrc) = I.PipeM (fmap (go xs) msrc)
    go xs (I.NeedInput i c)  
      | Just (x, xs') <- pop xs = go xs' (i x)
      | otherwise = I.NeedInput (go xs . i) (go xs . c)
    go xs (I.Leftover p l) = I.Leftover (go xs p) l
type Stack a = ([a],[a])
push :: a -> Stack a -> Stack a
push a (l,r) = (a:l, r)
pop :: Stack a -> Maybe (a, Stack a)
pop (a:xs, ys) = Just (a, (xs,ys))
pop ([], []) = Nothing
pop ([], ys) = pop (reverse ys, [])


v :: V.Vector Int
v  = V.fromList [ 3,8,1001,8,10,8,105,1,0,0,21,34,51,76,101,126,207,288,369,450,99999,3,9,102,4,9,9,1001,9,2,9,4,9,99,3,9,1001,9,2,9,1002,9,3,9,101,3,9,9,4,9,99,3,9,102,5,9,9,1001,9,2,9,102,2,9,9,101,3,9,9,1002,9,2,9,4,9,99,3,9,101,5,9,9,102,5,9,9,1001,9,2,9,102,3,9,9,1001,9,3,9,4,9,99,3,9,101,2,9,9,1002,9,5,9,1001,9,5,9,1002,9,4,9,101,5,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99 ]
