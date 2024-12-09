module Main where
import Data.Vector(Vector,fromList,(!?),(!))

getElem :: (Int,Int) -> Vector (Vector Char) -> Maybe Char
getElem (x,y) array = do
  tmp <- array !? y
  tmp !? x

-- I couldn't think of a good name but basically if you give it a coordinate and
-- a direction it'll give you a string
arrayLine :: (Int,Int) -> (Int,Int) -> Int -> Vector (Vector Char) -> Maybe [Char]
arrayLine (x,y) (dx,dy) len array = let tups = [(x+(dx*mul),y+(dy*mul)) | mul <- [0..len-1]]
                                        in sequence $ fmap (\pos -> getElem pos array) tups

checkXmas :: (Int,Int) -> Vector (Vector Char) -> Int
checkXmas point array = let directions = [(x,y) | x<- [-1..1], y<- [-1..1] ]
                            in length $ filter (== Just "XMAS")
                               $ fmap (\x -> arrayLine point x 4 array) directions


getGenericCount ::  Char -> ((Int,Int) -> Vector (Vector Char) -> Int) -> Vector (Vector Char) -> Int
getGenericCount c func array = let coors = [(y,x) | x<-[0..length array], y<-[0..length (array ! 0)]]
                                   coors2 = filter (\x-> getElem x array == Just c) coors
                               in foldr (+) 0 $ map (\x -> func x array) coors2

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (u, v) = (x+u, y+v)

rotateStr :: String -> String
rotateStr str = (last str:init str)

checkMMSS :: String -> Bool
checkMMSS str = length (filter (=="MMSS") $ take 4 $ iterate rotateStr str) /= 0

checkMas :: (Int,Int) -> Vector (Vector Char) -> Bool
checkMas pos array
  | getElem pos array == Just 'A' = Just True ==  do
      str <- sequence $ fmap (\x-> getElem (add x pos) array) dirs
      Just $ checkMMSS str
  | otherwise = False
  where dirs = [(-1,-1),(-1,1),(1,1),(1,-1)]


main :: IO ()
main = do
  content <- readFile "day4a.txt"
  array <- return $ fromList $ fmap fromList $ lines $ content
  print $ (getGenericCount 'X' checkXmas)  $ array
  print $ (getGenericCount 'A' (\x y-> if checkMas x y then 1 else 0)) $ array
