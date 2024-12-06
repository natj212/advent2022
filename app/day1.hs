import Data.List
import System.IO

pairDistance (x,y) = foldl (+) 0 $ map abs $  zipWith (-) (sort y) (sort x)

parseNums :: String -> [[Integer]]
parseNums str = map  ((map read) . words) $ lines str

invertList :: [[Integer]] -> ([Integer],[Integer])
invertList lst = (map (\x -> x !! 0) lst, map (\x -> x !! 1) lst)




 -- Assume lists are sorted
getCount :: Integer -> [Integer] -> Integer
getCount _ [] = 0
getCount num (x:xs)
  | num == x = 1 + getCount num xs
  | otherwise = getCount num xs

similarityScore :: ([Integer],[Integer]) -> Integer
similarityScore (leftLst,rightLst) = foldr (+) 0 $ map (\x -> x * getCount x rightLst) leftLst

main = do
  contents <- readFile "day1a.txt"
  print $ similarityScore $ invertList $ parseNums contents
  
  
