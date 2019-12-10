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
import Data.Functor (void)
import Data.List (maximumBy, permutations)
import Data.Ord

main :: IO ()
    main = do
       getMax =<< candidates [0..4] (\ls -> runConduit $ yield 0 .| steps ls .| lastC)
       getMax =<< candidates [5..9] (\ls -> runConduit $ yield 0 .| loop (steps ls) .| lastC)
      where
        steps = foldr1 (.|) . fmap step
        step x = leftover x >> void (runMachine program v)
        candidates vs f = sequence [fmap (ls,) (f ls) | ls <- permutations vs]
        getMax = print . maximumBy (comparing snd)

instance (Monad m, l ~ Int, r ~ Int) => MachineIO (ConduitT l r m) where
    input = fmap fromJust await
    output = yield

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
