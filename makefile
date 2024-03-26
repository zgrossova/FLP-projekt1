# Makefile for FLP 2023/2024 project in Haskell language 
# Zaneta Grossová (xgross11)
# 31.3.2024

GHC = ghc
FLAGS = -Wall

.PHONY: all clean

all: flp-fun

flp-fun: DecisionTree.hs TreeTraining.hs Main.hs
	$(GHC) $(FLAGS) $^ -o $@

# Clean build files
clean:
	rm -f flp-fun

