-- DecisionTree.hs / FLP 2023/2024 project in Haskell language 
-- Zaneta Grossová (xgross11)
-- 31.3.2024

module DecisionTree where

import Text.Printf


-- definition of DecisionTree data structure
data DecisionTree = EmptyTree
                    | Node Int Double DecisionTree DecisionTree 
                    | Leaf String

-- own Show instance
instance Show DecisionTree where
    show tree = showTreeIndented tree 0

showTreeIndented :: DecisionTree -> Int -> String
showTreeIndented EmptyTree _ = ""
showTreeIndented (Leaf l) indent = replicate (2 * indent) ' ' ++ "Leaf: " ++ l ++ "\n"
showTreeIndented (Node i f left right) indent =
    replicate (2 * indent) ' ' ++ "Node: " ++ show i ++ ", " ++ printf "%.4f" f ++ "\n" ++ showTreeIndented left (indent + 1) ++ showTreeIndented right (indent + 1)