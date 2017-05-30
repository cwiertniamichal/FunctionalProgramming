module SpecQC (runQC) where

import Test.QuickCheck

import Lib(l)

prop_id xs = l == [1]

runQC :: IO ()
runQC = do
    putStr "\n\nTrying QuickCheck:\n"
    putStr "===============================================\n"
    quickCheck (prop_id :: [Integer] -> Bool)
    putStr "===============================================\n\n\n"
