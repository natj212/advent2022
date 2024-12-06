checkDescending2 :: [Integer] -> [Integer] -> Bool -> Bool
checkDescending2 [] post happened = checkDescending2 [head post] (tail post) happened
checkDescending2 _ [] happened = True
checkDescending2 pre post happened
  | diffBool  = checkDescending2 (pre ++ [head post]) (tail post) happened
  | not diffBool && not happened = (checkDescending2 pre (tail post) True) ||
    (checkDescending2 (init pre) post True)
  | otherwise = False
    where diff = (head post) - (last pre)
          diffBool = (diff > 0) && (diff <= 3)


checkBoth :: [Integer] -> Bool
checkBoth lst = (checkDescending2 [] lst True) || (checkDescending2 [] (reverse lst) True)

checkBoth2 :: [Integer] -> Bool
checkBoth2 lst = (checkDescending2 [] lst False) || (checkDescending2 [] (reverse lst) False)

parseNums :: String -> [[Integer]]
parseNums str = map  ((map read) . words) $ lines str

countTrue :: [Bool] -> Integer
countTrue [] = 0
countTrue (x:xs) = if x then 1 + countTrue xs else countTrue xs

main = do
  contents <- readFile "day2a.txt"
  let part1 = countTrue $ map checkBoth $ parseNums $ contents
  print part1
  let part2 = countTrue $ map checkBoth2 $ parseNums $ contents
  print part2


