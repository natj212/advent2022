module Main where

import Text.Regex.TDFA


main :: IO ()
main = putStrLn "Hello, Haskell!"

getMults :: String -> [String]
getMults str = getAllTextMatches ( str =~ "" )

