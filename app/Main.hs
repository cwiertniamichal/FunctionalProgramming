module Main where

import CParser 
import System.Environment

main :: IO ()
main = do 
    args <- getArgs
    let file = if args == [] then "files/trial1.txt" else head args
    putStrLn $ "Parsing " ++ file ++ "\n"
    ast <- parseProgram file
    putStr $ "AST:\n" ++ show(ast)

