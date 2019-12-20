{-# Language ScopedTypeVariables #-}
{-# Language TupleSections #-}
module Grid where
import qualified Data.List as L
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S
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
left (a,b) = (-b, a)
right (a,b) = (b, -a)

data Spot a = Passage | Wall | POI a
  deriving (Eq, Ord, Show)
data Graph a = Graph { unGraph :: M.Map a [(a,Int)]}
  deriving (Eq, Ord, Show)
neighborsG :: Ord a => Graph a -> a -> [(a,Int)]
neighborsG (Graph g) a 
  | Just out <- g M.!? a = out
  | otherwise = []

-- parseGrid :: forall a b. Ord b => M.Map (Int, Int) a -> ((Int, Int) -> a -> Spot b) -> Graph b
parseGrid m check = Graph $ M.fromList [(b, [(t, cost) | (Left t,cost) <- from p, t /= b])| (p, a) <- M.toList m, POI b <- pure (check p a)]
  where
    -- from :: (Int, Int) -> [(b, Int)]
    from p = (dijkstra 0 (+) step (Right p))
    -- step :: Either b (Int, Int) -> [(Either b (Int, Int), Int)]
    step (Left _) = []
    step (Right p) = (,1) <$> concatMap applyCheck (adjacent p)
    applyCheck a
      | Just n <- m M.!? a = case check a n of
        Wall -> []
        Passage -> [Right a]
        POI b -> [Left b]
      | otherwise = undefined
    -- ofInterest 

data PQ a = PQ a (PQ a) (PQ a) | Empty
  deriving Show

singleton :: a -> PQ a
singleton a = PQ a Empty Empty
merge :: Ord a => PQ a -> PQ a -> PQ a
merge Empty a = a
merge a Empty = a
merge l@(PQ lv l1 r1) r@(PQ rv l2 r2)
  | lv <= rv = PQ lv (merge r r1) l1
  | otherwise = PQ rv (merge l r2) l2

insert :: Ord a => a -> PQ a -> PQ a
insert = merge . singleton

getMin :: Ord a => PQ a -> Maybe (a, PQ a)
getMin Empty = Nothing
getMin (PQ a l r) = Just (a, merge l r)

fromList :: Ord a => [a] -> PQ a
fromList = foldr insert Empty
dijkstra c0 = aStar (const c0) c0
aStar :: (Ord s, Ord c) => (s -> c) -> c -> (c -> c -> c) -> (s -> [(s, c)]) -> s-> [(s,c)]
aStar heuristic cost0 addCosts step s0 =  tail $ go (singleton (heuristic s0, cost0, s0)) (S.empty)
  where
    flip (a,b) = (b,a)
    go h seen = case getMin h of
      Just ((_, c, s), h')
        | not (S.member s seen) -> (s,c) : go (toQ s c (step s) `merge` h') (S.insert s seen)
        | otherwise -> go h' seen
      Nothing -> []
    toQ f c ls = fromList $ map (\(a,c') -> let c'' = c' `addCosts` c in (heuristic a `addCosts` c'', c'', a)) ls
