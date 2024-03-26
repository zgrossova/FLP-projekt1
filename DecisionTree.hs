-- DecisionTree.hs / FLP 2023/2024 project in Haskell language 
-- Zaneta Grossová (xgross11)
-- 31.3.2024

module DecisionTree where

classification :: String -> String -> IO ()
classification treeFilePath dataFilePath = do
    treeFileContent <- readFile treeFilePath
    let tree = parseDecisionTree (lines treeFileContent)
    inputFileContent <- readFile dataFilePath
    classify (lines inputFileContent) tree


----- Part 1: Create desision tree -----

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

splitTree :: [String] -> ([String], [String])
splitTree [] = ([], [])
splitTree (line:rest) =
    let indent = countIndentation line
        (thisLevel, nextLevels) = span (\l -> countIndentation l > indent) rest
    in (line:thisLevel, nextLevels)

countIndentation :: String -> Int
countIndentation str = length (takeWhile (\c -> c == ' ') str)

----- Part 2: Classify new data based on created tree -----

classify :: [String] -> DecisionTree -> IO()
classify _ EmptyTree = putStr ""
classify [] tree = putStr ""
classify (line:rest) tree = do 
    let features = splitOn (==',') line
    putStrLn (evaluate features tree)
    classify rest tree

evaluate :: [String] -> DecisionTree -> String
evaluate _ (Leaf l) = l
evaluate features (Node i f left right) 
    | length features < i = error "Feature index out of range"
    | read (features !! i) <= f = evaluate features left
    | otherwise = evaluate features right

splitOn :: (Char -> Bool) -> String -> [String]
splitOn sep list = case dropWhile sep list of
                      "" -> []
                      list' -> w : splitOn sep list''
                            where (w, list'') = break sep list'