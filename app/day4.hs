module Main where
import Data.Vector(Vector,fromList,(!?),(!))
-- data Pos = Pos {x::Int,
--                 y::Int} deriving (Show)

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


getXmasCount :: Vector (Vector Char) -> Int
getXmasCount array = let coors = [(y,x) | x<-[0..length array], y<-[0..length (array ! 0)]]
                         coors2 = filter (\x-> getElem x array == Just 'X') coors
                               in foldr (+) 0 $ map (\x -> checkXmas x array) coors2

main :: IO ()
main = do
  content <- readFile "day4a.txt"
  print $ getXmasCount $ fromList $ fmap fromList $ lines $ content
