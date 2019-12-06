module Aoc19_4 where
    import Data.List (group)
    import Control.Applicative (liftA2)

    main :: IO ()
    main = do
      let ls = map show [138241..674034::Int]
          isIncreasing = and . (zipWith (>=) =<< tail)
          solve = fmap (print . length)
                . filter . (liftA2 (&&) isIncreasing) . (. group) . any . (. length)
      solve (>= 2) ls
      solve (== 2) ls
