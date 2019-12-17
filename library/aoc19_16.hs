module Aoc19_16 where
import Data.Char


mults :: Int -> [Int]
mults i = tail $ cycle $ concatMap (replicate i) $ [0,1,0,-1]


apply :: [Int] -> Int -> Int
apply vals i = (`mod`10) $ abs $ sum $ zipWith (*) (mults i) vals


step :: [Int] -> [Int]
step v = map (apply v) [1..length v]

input :: [Int]
input = map digitToInt "59750939545604170490448806904053996019334767199634549908834775721405739596861952646254979483184471162036292390420794027064363954885147560867913605882489622487048479055396272724159301464058399346811328233322326527416513041769256881220146486963575598109803656565965629866620042497176335792972212552985666620566167342140228123108131419565738662203188342087202064894410035696740418174710212851654722274533332525489527010152875822730659946962403568074408253218880547715921491803133272403027533886903982268040703808320401476923037465500423410637688454817997420944672193747192363987753459196311580461975618629750912028908140713295213305315022251918307904937"

main = print $ foldl toNum 0 $ take 8 $ (iterate step input) !! 100
 where toNum acc cur = acc * 10 + cur