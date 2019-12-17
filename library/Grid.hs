module Grid where
import Data.List
import Data.Ord
import qualified Data.Map as M
indexGrid :: [[a]] -> M.Map (Int, Int) a
indexGrid lls = M.fromList $ [ ((x,y), l) | (y,ls) <- zip [0..] lls, (x,l) <- zip [0..] ls]

plusPoint (a,b) (x,y) = (a+x,b+y)
showGrid :: (Enum b, Enum a1, Ord b, Ord a1) => ((a1, b) -> Maybe a3 -> Char) -> M.Map (a1, b) a3 -> IO ()
showGrid f b = mapM_ putStrLn [[ f (x,y) (b M.!? (x,y))| x <- [lx..rx]] | y <- [ly..ry]]
  where
    lx = minimum $ map fst $ M.keys b
    rx = maximum $ map fst $ M.keys b
    ly = minimum $ map snd $ M.keys b
    ry = maximum $ map snd $ M.keys b

adjacent :: (Num b, Enum b, Eq b) => (b, b) -> [(b, b)]
adjacent (px, py) = [(px,py-1), (px+1, py), (px, py+1), (px-1, py)]

manhattan (a,b) (x,y) = abs (a-x) + abs (b-y)
