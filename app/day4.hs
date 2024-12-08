module Main where
import Data.List.Extra ((!?))

-- data Pos = Pos {x::Int,
--                 y::Int} deriving (Show)

getElem :: (Int,Int) -> [String] -> Maybe Char
getElem (x,y) array = do
  tmp <- array !? y
  tmp !? x

-- addPos :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- addPos x y = uncurry bimap (both (+) x) y

-- I couldn't think of a good name but basically if you give it a coordinate and
-- a direction it'll give you a string
-- arrayLine :: (Int,Int) -> (Int,Int) -> Int -> [String] -> Maybe String
-- arrayLine _ _ 0 _ = Just ""
-- arrayLine pos dir len array = do
--   head' <- getElem pos array
--   tail' <- arrayLine (addPos pos dir) dir (len - 1) array
--   Just (head':tail')

arrayLine :: (Int,Int) -> (Int,Int) -> Int -> [String] -> Maybe [Char]
arrayLine (x,y) (dx,dy) len array = let tups = [(x+(dx*mul),y+(dy*mul)) | mul <- [0..len-1]]
                                        in sequence $ fmap (\pos -> getElem pos array) tups

main :: IO ()
main = do
  content <- readFile "day4test.txt"
  print $ arrayLine (0,4) (1,0) 4  $ lines $ content
