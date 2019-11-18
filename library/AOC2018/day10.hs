{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day10 where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Control.Lens as L
import Control.Lens.Regex.Text
import Numeric.AD as AD
import qualified Data.List as L
import Data.Function (on)

data Pos a = Pos { posX:: a, posY :: a , velX :: a, velY ::  a }
    deriving Show

main :: IO ()
main = do
  c <- T.readFile "./library/AOC2018/input10"
  let a = fixpoint c 0
  T.putStrLn $ printS $ calcAt c a
  print a

fixpoint :: Text -> Double -> Double
fixpoint input n0 = go n0 iters
  where
    go n 0 = n
    go n i = go (step n i) (i-1)

    costFun :: (Floating a, Ord a) => a -> a
    costFun t = boundingBoxSize $ fmap (posAt t) dat
      where dat = parse input

    gradAt :: Double -> Double
    gradAt = diff costFun

    step n i = n - normalize (gradAt n) i
    normalize v i = v * (1-(fromIntegral iters-fromIntegral i)/fromIntegral iters)

    iters :: Int
    iters = 1500


posAt :: Num a => a -> Pos a -> Pos a
posAt time Pos{..} = Pos {posX = posX + velX * time, posY = posY + velY * time, ..}

calcAt :: RealFrac a => T.Text -> a -> [Pos Int]
calcAt input t = fmap (posAt (round t)) (parse input)


parse :: Num a => Text -> [Pos a]
parse t =t ^.. [regex|position=< *(-?\d*), *(-?\d*)> *velocity=< *(-?\d*), *(-?\d*)>|] . groups . to parseSingle
  where
   parseSingle = toPos . fmap (fromInteger . read . T.unpack)
   toPos [a,b,c,d] = Pos a b c d
   toPos _ = error ""


boundingBoxSize :: (Floating a, Ord a) => [Pos a] -> a
boundingBoxSize  ls =  (maxX - minX) + (maxY - minY)
  where (minX, maxX, minY, maxY) = boundingBox ls

boundingBox :: (Ord a) => [Pos a] -> (a,a,a,a)
boundingBox (Pos x0 y0 _ _:ls) = foldl step (x0, x0, y0, y0) ls
  where
    step (a,b,c,d) (Pos px py _ _)= (min a (px), max b (px), min c (py), max d (py))
boundingBox []  = undefined

data Step = Step Double Double Double
 deriving Show

groupingsBy :: (Ord b) => (a -> b) -> [a] ->  [(b, [a])]
groupingsBy f = fmap (\a -> (f (head a), a)).  L.groupBy ((==)`on`(f)) . L.sortOn (f)
printS :: [Pos Int] -> T.Text
printS ls = formatOuter ls
  where
    (minX, maxX, minY, _maxY) = boundingBox ls

    formatOuter = T.unlines . formatBlock minY . groupingsBy posY
    formatBlock _ [] = []
    formatBlock i w@((pY, line):rs) = case compare i pY of
       EQ ->  formatInner line : formatBlock (i+1) rs
       LT -> T.replicate (maxX - minX) " " : formatBlock (i+1) w
       GT -> formatBlock i rs
    formatInner = T.pack . formatLine minX .  L.sortOn posX
    formatLine :: Int -> [Pos Int] -> String
    formatLine i w@(Pos {posX}:rs) = case compare i posX of
       EQ -> '#' : formatLine (i+1) rs
       LT -> ' ' : formatLine (i+1) w
       GT -> formatLine i rs
    formatLine _ [] = []
