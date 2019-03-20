{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module ProblemState where

class ProblemState s a | s -> a where
    successors :: s -> [(a, s)]
    isGoal :: s -> Bool
    heuristic :: s -> Int
    heuristic = const 0
