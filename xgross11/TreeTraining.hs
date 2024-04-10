-- TreeTraining.hs / FLP 2023/2024 project in Haskell language 
-- Zaneta Grossová (xgross11)
-- 31.3.2024

module TreeTraining where

import DecisionTree
import Helper (splitOn)
import Data.List (sortBy, groupBy, sort, group, partition)

type Sample = ([Double], String)
type TrainingData = [Sample]


-- is called from main
-- parses input, build tree on the dataset and show it on output
training :: String -> IO ()
training trainingFilePath = do
    trainingFileContent <- readFile trainingFilePath
    let dataset = parseDataset (lines trainingFileContent)
    let tree = buildTree dataset
    putStr(show tree)

-- [resursive] build DecisionTree from given data
-- receives dataset
-- calls trainTree that iterates through attributes, for each calculate best split and returns list of these splits
-- then split dataset by treshold that had best GINI index to <= and > 
-- If GINI is 0 and both subsets contains only one class, Leaf is created
-- otherwise is created Node with treshold with lowest GINI and recursivly run for both subtrees 
buildTree :: TrainingData -> DecisionTree
buildTree [] = EmptyTree
buildTree trainingData = 
    let splits = trainTree trainingData
        (index, bestGini, thresholdAndClass) = head $ sortByBestGini splits
        threshold = fst thresholdAndClass
        classLabel = snd thresholdAndClass
        -- split whole dataset by threshold in index attribute
        (lessOrEqual, greater) = partition (\(features, _) ->
                                    let featureValue = (features !! index)
                                    in featureValue <= threshold) trainingData
        leftSubtree = buildTree lessOrEqual
        rightSubtree = buildTree greater    
    in
        if bestGini == 0.0 
            then if (hasSameClass trainingData)
                    then Leaf classLabel
                    else Node index threshold leftSubtree rightSubtree
            else Node index threshold leftSubtree rightSubtree


-- receive trainingData
-- returns true if all samples belongs to the same class
hasSameClass :: TrainingData -> Bool
hasSameClass [] = True
hasSameClass [_] = True
hasSameClass ((_, class1):(features, class2):rest) = class1 == class2 && hasSameClass ((features, class2):rest)


-- receives dataset
-- iterates over each attribute, extracts its values to list of tuples representing each row (attr_value, class)
-- find best split for each attribute
-- returns list of triplets (index_attribute, bestGini, (threshold, class))
trainTree :: TrainingData -> [(Int, Double, (Double, String))]
trainTree [] = []
trainTree dataset = 
    map (\i -> findBestSplit 
    (sortAndGroup (getFeatureValuesWithClass dataset i)) 
    (getFeatureValuesWithClass dataset i) i 1 (0, "0")) 
    [0..numAttributes ]
    where
        numAttributes = length (fst (head dataset)) - 1 -- last attribute is class


-- receives lines of input file
-- returns TrainingData - list of tuples ([features], class)
parseDataset :: [String] -> TrainingData
parseDataset [] = []
parseDataset (line:rest) = 
    (map read(init features) :: [Double], classLabel) : (parseDataset rest)
    where
        features = splitOn (==',') line
        classLabel = last features


-- [resursive] finds best split for given attribute
-- receives list of sorted groupes, where each group contains tuples with the same feature value,
--          list of all groupes (for GINI index calculation),
--          index of attribute, 
--          best GINI so far
--          tuple (featureValue, correspondingClass)
-- returns triplet (index_attribute, bestGini, (threshold, class))
-- treshold is counted as avarage of current and next threshold, or just current treshold for last feature
-- returned newGini index is multiplied by ratio of size of subset / size of whole set
findBestSplit :: [[(Double, String)]] -> [(Double, String)] -> Int -> Double -> (Double, String) -> (Int, Double, (Double, String))
findBestSplit [] _ index bestGini thresholdAndClass = (index, bestGini, thresholdAndClass)
findBestSplit [firstGroup] allGroups index bestGini thresholdAndClass = 
    let 
        (smallerOrEqual, greater) = splitGroup allGroups (head firstGroup)
        newGini = ((fromIntegral (length smallerOrEqual) * calculateGiniIndex smallerOrEqual) 
                    + (fromIntegral (length greater) * calculateGiniIndex greater))
                    / fromIntegral (length allGroups)
        (newBestGini, newThresholdandClass) =
            if newGini < bestGini
                then (newGini, head firstGroup)
                else (bestGini, thresholdAndClass)
    in 
        findBestSplit [] allGroups index newBestGini newThresholdandClass
findBestSplit (firstGroup:secondGroup:rest) allGroups index bestGini thresholdAndClass =
    let 
        (smallerOrEqual, greater) = splitGroup allGroups (head firstGroup)
        newGini = ((fromIntegral (length smallerOrEqual) * calculateGiniIndex smallerOrEqual) + (fromIntegral (length greater) * calculateGiniIndex greater)) / fromIntegral (length allGroups)
        (newBestGini, newThresholdandClass) =
            if newGini < bestGini
                then if null secondGroup
                    then (newGini, head firstGroup)
                    else (newGini, average)
                else (bestGini, thresholdAndClass)
            where
                average = ((fst (head firstGroup) + fst (head secondGroup)) / 2, snd (head firstGroup))
    in 
        findBestSplit (secondGroup:rest) allGroups index newBestGini newThresholdandClass


-- receives list of tuples (value, class) representing one feature
-- returns GINI index of given subset, for weighted GINI index is needed to multiple it
-- by ratio of size of subset / size of whole set (in findBestSplit)
calculateGiniIndex :: [(Double, String)] -> Double
calculateGiniIndex attributeData = 
    1.0 - sum (map (\prob -> (prob*prob)) (computePropabilities (countOccurrences (map snd attributeData)) (length attributeData)))


-- receive trainingData
-- returns true if all samples belongs to the same class
sortByBestGini :: [(Int, Double, (Double, String))] -> [(Int, Double, (Double, String))]
sortByBestGini = sortBy (\(_, gini1, _) (_, gini2, _) -> compare gini1 gini2)

-- receives list of items
-- returns counts of occurences of each item in that list
countOccurrences :: Ord a => [a] -> [Int]
countOccurrences xs = map length (group $ sort xs)

-- receives list of occurences of each class
-- each item devide by size of whole dataset
-- returns list of propabilities
computePropabilities :: [Int] -> Int -> [Double]
computePropabilities [] _ = []
computePropabilities list 0 = replicate (length list) 0.0
computePropabilities (x:xs) totalObservations = ( fromIntegral x / fromIntegral totalObservations ) : computePropabilities xs totalObservations


-- receives TrainingData and index of feature
-- returns list of tuples of feature value and corresponding class (filter feature index and class from each row)
getFeatureValuesWithClass :: TrainingData -> Int -> [(Double, String)]
getFeatureValuesWithClass [] _ = []
getFeatureValuesWithClass ((features, classLabel) : xs) index = ((features !! index), classLabel) : getFeatureValuesWithClass xs index


-- receives output of getFeatureValuesWithClass - filtered feature index with corresponding classes in tuples
-- returns sorted and grouped input - list of lists representing each group, 
-- sorts and group by first item from tuple (by value of feature)
sortAndGroup :: [(Double, String)] -> [[(Double, String)]]
sortAndGroup attributeData = groupBy (\a b -> fst a == fst b) (sortTupleByFirst attributeData)


-- Sorts a list of tuples by the first element of each tuple
sortTupleByFirst :: Ord a => [(a, b)] -> [(a, b)]
sortTupleByFirst = sortBy (\(x, _) (y, _) -> compare x y)

-- split groups of sorted attribute values by threshold value to <= and >
splitGroup :: [(Double, String)] -> (Double, String) -> ([(Double, String)], [(Double, String)])
splitGroup [] _ = ([], [])
splitGroup groupedData thresholdAndClass = (smallerOrEqual, greater)
    where
        threshold = fst thresholdAndClass
        smallerOrEqual = filter (\grp -> fst grp <= threshold) groupedData
        greater = filter (\grp -> fst grp > threshold) groupedData