module Aoc18_15 where
import qualified Data.Map as M
import Algorithm.Search
import Data.List (sortOn)
import Data.Ord
import qualified Data.Set as S

-- import Debug.Tr
trace = flip const

type Pos = (Int, Int)
data Move = MoveTo Int Pos Pos
  deriving (Show, Eq, Ord)
data Hit = Hit Int Pos
  deriving (Show, Eq, Ord)
data Terr = Wall | Free
  deriving (Show, Eq, Ord)

type Id = Int
data T = Goblin | Elf
  deriving (Show, Eq, Ord)
data Unit = U { getPos :: Pos, getHP :: Int, getT :: T}
  deriving (Show, Eq, Ord)
data Board = B { bTerrain :: M.Map Pos Terr, bUnits ::  (M.Map Id Unit) }
  deriving (Show, Eq, Ord)

main = do
   let
    rate (i,b) = sum (map getHP $ M.elems $ bUnits b) * i
    go b' = do
        mapM_ putStrLn (printBoard b')
        -- let U up _ ut = (bUnits b') M.! 0
        -- print ((bUnits b') M.! 0)
        -- print (findMove b'  up ut)
        c <- getChar
        if c == '\n'
        then go (stepState b')
        else return ()
    done b = all ((==Elf) . getT) (M.elems $ bUnits b) || all ((==Goblin) . getT) (M.elems $ bUnits $ b)
   let p = head $ dropWhile (not . done . snd) $  zip [0..] $ tail $ iterate stepState b
   -- print (M.toAscList u)
   -- print t
   -- print u
   -- go b
   mapM_ putStrLn (printBoard b)
   mapM_ putStrLn (printBoard (stepState b))
   -- print (fst p)
   -- print (bUnits $ snd $ p)
   -- print (rate p)


  where
    b = B t u
    t = foldr (uncurry parseTerrain) mempty (toList input)
    u = foldr (uncurry parseUnit) mempty (toList input)
printChar :: Pos -> Board -> Maybe Char
printChar p (B t u)
  | [u] <- filter ((==p) . getPos) (M.elems u) = if getT u == Goblin then Just 'G' else Just 'E'
  | Just Wall <- t M.!? p = Just '#'
  | Just Free <- t M.!? p = Just '.'
  | otherwise = Nothing
printBoard :: Board -> [String]
printBoard b = printLine 0
  where
     printLine i = case  printChar (i,0) b of
       Just c -> go i 0 : printLine (i+1)
       Nothing -> []
     go x y = case  printChar (x,y) b of 
        Just c -> c : go x (y+1)
        Nothing -> []
         

input = unlines [
        -- "#######",
        -- "#E..G.#",
        -- "#...#.#",
        -- "#.G.#G#",
        -- "#######"]
            -- "#######",--       #######   
            -- "#E..EG#",--       #.E.E.#   E(164), E(197)
            -- "#.#G.E#",--       #.#E..#   E(200)
            -- "#E.##E#",--  -->  #E.##.#   E(98)
            -- "#G..#.#",--       #.E.#.#   E(200)
            -- "#..E#.#",--       #...#.#   
            -- "#######"]--
                    -- "#######",--       #######   
                    -- "#E.G#.#",--       #G.G#.#   G(200), G(98)
                    -- "#.#G..#",--       #.#G..#   G(200)
                    -- "#G.#.G#",--  -->  #..#..#   
                    -- "#G..#.#",--       #...#G#   G(95)
                    -- "#...E.#",--       #...G.#   G(200)
                    -- "#######"]--       ####### 
            "#######",
            "#.G...#",
            "#...EG#",
            "#.#.#G#",
            "#..G#E#",
            "#.....#",
            "#######"]
          -- "################################",
          -- "##########..#####...############",
          -- "##########.G..####..############",
          -- "########.....##.##.#############",
          -- "####G..#G#.......#.#############",
          -- "#G.....#.GG...G.##.#############",
          -- "#.#...G.......#.##..############",
          -- "###..#.......#####......########",
          -- "######.......#####..G....#######",
          -- "######..GG..######.......#######",
          -- "#####.GG....##.####G......G.####",
          -- "###.............G...........####",
          -- "###.#.........#####G..G....#####",
          -- "###..#.##....#######E.....######",
          -- "########....#########.#######..#",
          -- "#########...#########.#######..#",
          -- "########....#########..##......#",
          -- "#########...#########...#...#.E#",
          -- "#########...#########.......##.#",
          -- "########....E#######.......#####",
          -- "######........#####....E.#######",
          -- "######.......E..E..G.E....######",
          -- "#######.............###....#####",
          -- "#######............####.E...####",
          -- "#######...G....E##....##....####",
          -- "#######...............##########",
          -- "############.E.......###########",
          -- "###########.....#....###########",
          -- "###########.....#....###########",
          -- "###########.....###.############",
          -- "###########.#.##################",
          -- "################################"]
parseTerrain :: Pos -> Char -> M.Map Pos Terr -> M.Map Pos Terr
parseTerrain p '#' m = M.insert p Wall m
parseTerrain p _ m = M.insert p Free m
parseUnit :: Pos -> Char -> M.Map Id Unit -> M.Map Id Unit
parseUnit p 'G' m = M.insert (M.size m) (U p 200 Goblin) m
parseUnit p 'E' m = M.insert (M.size m) (U p 200 Elf) m
parseUnit _ _ m = m
toList :: String -> [(Pos, Char)]
toList ls = sortOn Down [((y,x),c) |  (y,lls) <- zip [0..] (lines ls), (x, c) <- zip [0..] lls]
findMove :: Board -> Pos -> T -> [Move]
findMove b p0@(x0,y0) t = [m | m <- search]
  where
    search = case bfs step (\x -> S.member x targets) p0 of
      Just [] -> pure $ MoveTo 0 p0 p0
      Just [_] -> pure $ MoveTo 0 p0 p0
      Just ls ->    pure $ MoveTo (length ls) (last $ init ls) (head ls)
      _ -> []
    targets = S.fromList $ map getPos $ getTargets t (bUnits b)
    step0 (x,y) = [(x-1,y), (x, y-1), (x, y+1), (x+1,y)]
    step p  = filter isValid (step0 p)
    isValid p = p == p0  || ((M.findWithDefault Wall p (bTerrain b)) == Free
                 && all ((/=p) . getPos) (getAllies t (bUnits b)))

noneAt :: Pos -> M.Map a Unit -> Bool
noneAt p m = null $ filter ((==p) . getPos) $ M.elems m
getTargets :: T -> M.Map a Unit -> [Unit]
getTargets t m = filter ((/= t) . getT) $ (M.elems m)
getAllies :: T -> M.Map a Unit -> [Unit]
getAllies t m = filter ((== t) . getT) $ (M.elems m)

findHit :: Board -> Pos -> T -> [Hit]
findHit b (l,r) t = [Hit hp p | (U p hp _) <- getTargets t (bUnits b), adjacent p]
  where
     adjacent (x,y) = abs(l-x) + abs (y-r) == 1

stepState :: Board -> Board
stepState b = foldl step b (M.toAscList (bUnits b))
  where
    -- step :: (Id, Unit) -> Board -> Board
    step  acc (i, u)
      | Just u' <- bUnits acc M.!? i  = stepUnit (i, u') acc -- already dead
      | otherwise = acc

stepUnit :: (Id, Unit) -> Board -> Board
stepUnit (i, u@(U pos _hp t)) b =  applyAttack hits b'
  where
    u' = applyMove (findMove b pos t) u
    b' = b { bUnits = (M.insert i u' (bUnits b)) }
    hits = findHit b' (getPos u') (getT u')
applyAttack :: [Hit] -> Board -> Board
applyAttack [] b = b
applyAttack ls b
 | trace (show (Hit hp pos) <> show u) False = undefined
 | hp < 3 = b { bUnits =  M.delete i (bUnits b) }
 | otherwise  = b { bUnits = (M.insert i (u { getHP = getHP u - 3})  (bUnits b)) }
  where
   (i, u) = head $ filter ((==pos) . getPos . snd) $ M.toList (bUnits b)
   Hit hp pos = minimum ls
    
     
applyMove :: [Move] -> Unit -> Unit
applyMove [] u = u
applyMove ls u 
   | trace (show (ls') <> show u)  True = u { getPos = p' }
  where
    ls' = minimum ls
    MoveTo _ _ p' = ls'
