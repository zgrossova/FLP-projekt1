-- TreeTraining.hs / FLP 2023/2024 project in Haskell language 
-- Zaneta Grossová (xgross11)
-- 31.3.2024

module TreeTraining where

training :: String -> IO ()
training trainingFilePath = do
    treeFileContent <- readFile trainingFilePath
    putStr "hello"