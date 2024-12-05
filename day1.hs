import Data.List
import System.IO

pairDistance (x,y) = foldl (+) 0 $ map abs $  zipWith (-) (sort y) (sort x)

parseNums :: String -> [[Integer]]
parseNums str = map  ((map read) . words) $ lines str

invertList :: [[Integer]] -> ([Integer],[Integer])
invertList lst = (map (\x -> x !! 0) lst, map (\x -> x !! 1) lst)



main = do
  contents <- readFile "day1a.txt"
  print $ pairDistance $ invertList $ parseNums contents
  
  
