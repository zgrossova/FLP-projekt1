-- Classification.hs / FLP 2023/2024 project in Haskell language 
-- Zaneta Grossová (xgross11)
-- 31.3.2024

module Classification where

import DecisionTree
import Helper (splitOn)


-- is called from main
-- parses input tree, creates decision tree
-- parses and classify data from input file
classification :: String -> String -> IO ()
classification treeFilePath dataFilePath = do
    treeFileContent <- readFile treeFilePath
    let tree = parseDecisionTree (lines treeFileContent)
    inputFileContent <- readFile dataFilePath
    classify (lines inputFileContent) tree


----- Part 1: Create desision tree -----


-- receives lines of input tree from file
-- split lines to words, recursively creates Nodes and Leafs
parseDecisionTree :: [String] -> DecisionTree
parseDecisionTree [] = EmptyTree
parseDecisionTree (line:rest) =
    case words line of
        ("Node:":index:threshold:_) ->
            let (leftLines, rightLines) = splitTree rest
                leftSubtree = parseDecisionTree leftLines
                rightSubtree = parseDecisionTree rightLines
            in Node (read (filter (/=',')index)) (read threshold) leftSubtree rightSubtree
        ("Leaf:":className:_) -> Leaf className
        _ -> error "Invalid tree format"


-- receives lines of input decision tree
-- split lines based on indention on left and right 'subtrees'
splitTree :: [String] -> ([String], [String])
splitTree [] = ([], [])
splitTree (line:rest) = (line:thisLevel, nextLevels)
    where
        indent = countIndentation line
        (thisLevel, nextLevels) = span (\l -> countIndentation l > indent) rest


-- count number of spaces in given string
countIndentation :: String -> Int
countIndentation str = length (takeWhile (\c -> c == ' ') str)

----- Part 2: Classify new data based on created tree -----


-- split input lines
-- iterates over each line, splits line by coma on each feature and evalueate its class
classify :: [String] -> DecisionTree -> IO()
classify _ EmptyTree = putStr ""
classify [] _ = putStr ""
classify (line:rest) tree = do 
    let features = splitOn (==',') line
    putStrLn (evaluate features tree)
    classify rest tree

-- receives list of features values
-- recursively calculates if value of feature is lessEqual or more than threshold of current node
-- based on that choose left or right subree, until Leaf is not reached
evaluate :: [String] -> DecisionTree -> String
evaluate _ (Leaf l) = l
evaluate _ EmptyTree = ""
evaluate [] _ = ""
evaluate features (Node i f left right) 
    | length features < i = error "Feature index out of range"
    | read (features !! i) <= f = evaluate features left
    | otherwise = evaluate features right