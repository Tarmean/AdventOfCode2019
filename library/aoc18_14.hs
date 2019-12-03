{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language TypeApplications #-}
{-# Language RankNTypes #-}
module Aoc18_14 where
import Debug.Trace
import qualified Data.Massiv.Array as A
import Control.Monad.ST
import qualified Data.Massiv.Array.Unsafe as U
import qualified Data.Massiv.Array.Mutable as A
import Control.Monad
import Data.Char as C


type Board s = A.MArray s A.U A.Ix1 Int

data State  s = S (Board s) Int Int Int


toDigits :: Int -> [Int]
toDigits = map digitToInt . show

stepState :: (A.PrimMonad m, s ~ A.PrimState m) => State s -> m (State s)
stepState (S b e1 e2 s) = do
   d1 <- A.read' b e1
   d2 <- A.read' b e2
   let
       dig = toDigits (d1+d2)
       s' = s + length dig
       e1' = (e1 + d1 + 1) `mod` s'
       e2' = (e2 + d2 + 1) `mod` s'
   
   b' <- if (getSz b  <= s')
        then U.unsafeLinearGrow b (A.Sz $ getSz b * 2)
        else pure b
   forM_ (zip [0..] dig) $ \(i, v) -> do
      A.write' b' (i + s) v
   pure $  S b' e1' e2' s'
 where getSz a = case A.msize a of A.Sz1 i -> i

-- loop :: State -> [Int]

runStateS :: (A.PrimMonad m) => State (A.PrimState m) -> m (A.Array A.U A.Ix1 Int)
runStateS (S b _ _ i) = do
  a <- (A.freezeS b)
  pure $ A.computeS $ A.takeS (A.Sz i) a
loopEnd ::  (A.PrimMonad m, s ~ A.PrimState m) => Int -> Int -> State s -> m (State s)
loopEnd n m s0 = (go s0)
    -- a -> a
  where
    go s
      | sLen s >= (n+m) =  pure s
      | otherwise = go =<< (stepState s) 
takeDigs m n = A.toList . A.computeS @A.U .  A.takeS (A.Sz m) . A.dropS (A.Sz n)
sLen (S _ _ _ l) = l

matches i a b = loop 0
  where
    loop j
      | j >= 6 = True
      | otherwise =
       let l = a A.!  (i + j)
           r = b  A.! i
       in
           if a /= b 
           then False
           else loop (j+1)

loopPat ::  (A.PrimMonad m, s ~ A.PrimState m) => A.Array A.U A.Ix1 Int -> State s -> m (State s)
loopPat pat s0 = (go s0)
    -- a -> a
  where
    go s = do
      s' <- (stepState s) 
      ls <- mapM (test 0 s') [sLen s .. sLen s']
      if or ls
      then pure s'
      else go s'
    patSize = (A.elemsCount pat)
    test i s@(S arr _ _ _) l
      | l > 21316380 = pure True
      | l < patSize  = pure False
      | i >= patSize = pure True
      | otherwise = do
        x <- A.read' arr (l - (patSize - i))
        let y = pat A.! i
        if l >= 20316370 && l <= 20316373
        then trace (show l <> ", " <> show i <> ", " <> show x <> "=" <> show y) (return ())
        else (return ())
        if (x /= y)
        then pure False
        else test (i+1) s l

main :: IO ()
main = do 
    let 
      pat :: A.Array A.U A.Ix1 Int
      pat = (A.fromList A.Seq [3,2,0,8,5,1])
    let out  = runST $ do
                            arr <- (A.thawS $ A.fromList A.Seq [3,7])
                            o <- (loopPat pat (S arr 0 1 2))
                            runStateS o
    print (A.elemsCount out - A.elemsCount pat)
    print $ takeDigs 10 20316365 out
