import Text.Regex.TDFA


getMult :: String -> (String,String,String,[String])
getMult str =  str =~ "mul\\(([[:digit:]]+),([[:digit:]]+)\\)"

thrd (_,_,z,_) = z
frth (_,_,_,z) = z
snd4 (_,z,_,_) = z

getMults :: String -> [[Int]]
getMults [] = []
getMults str = let res = getMult str
                   in if null (snd4 res)  then []
                      else (map read (frth res): getMults (thrd res))


main = do
  contents <- readFile "day3a.txt"
  print $ foldr (+) 0 $ fmap (foldr (*) 1) $ getMults contents

