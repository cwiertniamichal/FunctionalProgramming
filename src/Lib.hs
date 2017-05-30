module Lib
    ( someFunc,
    foo,
    l
    ) where

foo 3 = (1,2)
foo 4 = (2,2)

l = [1]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
