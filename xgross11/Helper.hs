-- Helper.hs / FLP 2023/2024 project in Haskell language 
-- Zaneta Grossová (xgross11)
-- 31.3.2024

module Helper where

splitOn :: (Char -> Bool) -> String -> [String]
splitOn sep list = case dropWhile sep list of
                      "" -> []
                      list' -> w : splitOn sep list''
                            where (w, list'') = break sep list'