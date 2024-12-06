import Text.Regex.TDFA


getMult :: String -> (String,String,String,[String])
getMult str =  str =~ "mul\\(([[:digit:]]+),([[:digit:]]+)\\)"

thrd4 (_,_,z,_) = z
frth4 (_,_,_,z) = z
snd4 (_,z,_,_) = z

thrd3 (_,_,z) = z
snd3 (_,z,_) = z

getMults :: String -> [[Int]]
getMults [] = []
getMults str = let res = getMult str
                   in if null (snd4 res)  then []
                      else (map read (frth4 res): getMults (thrd4 res))

getNextFunction :: String -> (String,String,String)
getNextFunction str = str =~ "mul\\([[:digit:]]+,[[:digit:]]+\\)|do\\(\\)|don\'t\\(\\)"


getFunctions :: String -> [String]
getFunctions str = let res = getNextFunction str
                       in if null (snd3 res) then []
                          else ((snd3 res) : getFunctions (thrd3 res))

processFunctions :: [String] -> Bool -> Int
processFunctions [] _ = 0
processFunctions (x:xs) enabled
  | x =~ "mul" && enabled =  (foldr (*) 1 $ head $ getMults x) + processFunctions xs enabled
  | x =~ "mul" && not enabled = processFunctions xs enabled
  | x =~ "don't" = processFunctions xs False
  | x =~ "do" = processFunctions xs True
  | otherwise = 0


main = do
  contents <- readFile "day3test.txt"
  let part1 = foldr (+) 0 $ fmap (foldr (*) 1) $ getMults contents
  print part1
  contents2 <- readFile "day3a.txt"
  let part2 =  processFunctions (getFunctions $ contents2) True
  print part2

