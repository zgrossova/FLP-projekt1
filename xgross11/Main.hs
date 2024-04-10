-- Main.hs / FLP 2023/2024 project in Haskell language 
-- Zaneta Grossová (xgross11)
-- 31.3.2024

module Main where

import qualified TreeTraining
import qualified Classification
import qualified System.Environment as E

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
    ["-1", treeFile, dataFile] -> Classification.classification treeFile dataFile
    ["-2", trainDataFile] -> TreeTraining.training trainDataFile
    _ -> do
        putStrLn "Incorrect arguments, see the usage."
        printHelp


-- Main program
main :: IO ()
main = do
    args <- E.getArgs
    parseArgs args
