-- Main.hs / FLP 2023/2024 project in Haskell language 
-- Zaneta Grossová (xgross11)
-- 31.3.2024

module Main where

import qualified DecisionTree
import qualified TreeTraining
import qualified System.IO
import qualified System.Environment as E


data DecisionTree = EmptyTree
                    | Node Int Double DecisionTree DecisionTree 
                    | Leaf String

instance Show DecisionTree where
    show tree = showTreeIndented tree 0

showTreeIndented :: DecisionTree -> Int -> String
showTreeIndented EmptyTree _ = ""
showTreeIndented (Leaf l) indent = replicate (2 * indent) ' ' ++ "Leaf: " ++ l ++ "\n"
showTreeIndented (Node i f left right) indent =
    replicate (2 * indent) ' ' ++ "Node: " ++ show i ++ ", " ++ show f ++ "\n" ++ showTreeIndented left (indent + 1) ++ showTreeIndented right (indent + 1)

-- function to print Help
printHelp :: IO ()
printHelp = do
    putStrLn "flp-fun 2023/2024"
    putStrLn "This program has two run options"
    putStrLn "  -1 <file with three> <file with new data>"
    putStrLn "  -2 <file with training data>"
    putStrLn "  -h prints this menu"

-- function that parses input arguments
parseArgs :: [String] -> IO () 
parseArgs args = case args of
    [] -> do 
        putStrLn "Empty arguments, see the usage."
        printHelp
    ["-1", treeFile, dataFile] -> DecisionTree.classification treeFile dataFile
    ["-2", trainDataFile] -> TreeTraining.training trainDataFile
    _ -> do
        putStrLn "Incorrect arguments, see the usage."
        printHelp


-- Main program
main :: IO ()
main = do
    args <- E.getArgs
    parseArgs args
