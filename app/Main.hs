module Main where

import CParser

main :: IO ()
main = do 
    ast <- parseProgram "files/trial1.txt" 
    print $ "AST: " ++ show(ast)

