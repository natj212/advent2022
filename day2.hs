checkDescending :: [Integer] -> Bool
checkDescending (x1:x2:xs) = if (diff > 0) && (diff <= 3) then checkDescending (x2:xs)
  else False
  where
    diff = x2 - x1
checkDescending (x1:[]) = True

checkBoth :: [Integer] -> Bool
checkBoth lst = (checkDescending lst) || (checkDescending $ reverse lst)

parseNums :: String -> [[Integer]]
parseNums str = map  ((map read) . words) $ lines str

countTrue :: [Bool] -> Integer
countTrue [] = 0
countTrue (x:xs) = if x then 1 + countTrue xs else countTrue xs

main = do
  contents <- readFile "day2test.txt"
  print $ countTrue $ map checkBoth $ parseNums $ contents

