module SpecQC (runQC) where

import Test.QuickCheck

--import Lib(l)
import CParser(parseProgram)

--prop_id xs = l == [1]
--prop_trial1 = parseProgram "files\trial1.txt" = FuncDef()

runQC :: IO ()
runQC = do
    putStr "\n\nTrying QuickCheck:\n"
    putStr "===============================================\n"
    --quickCheck (prop_id :: [Integer] -> Bool)
    putStr "===============================================\n\n\n"
