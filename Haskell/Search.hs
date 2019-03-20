{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Search where

import           Data.List
import           Data.Maybe
import qualified Data.Set     as S
import           ProblemState

data Node s a = Node { state    :: s
                     , action   :: Maybe a
                     , previous :: Maybe (Node s a)
                     , depth    :: Int
                }
                | NilNode
    deriving (Eq, Show)

nodeState :: Node s a -> s
nodeState = state

limitedDfs :: (ProblemState s a, Ord s)
           => s
           -> Bool
           -> Int
           -> [Node s a]
limitedDfs = \start eur limit ->
    let open = [(initial start)]
        initial = \ st -> (Node st Nothing Nothing 0)
        expand = \ node -> [ ( Node (snd x)
                                    (Just $ fst x)
                                    (Just node)
                                    ((depth node) + 1) ) | x <- if eur then sortBy (\n1 n2 -> compare (heuristic $ snd n1) (heuristic $ snd n2)) $ successors $ state node
                                                                       else successors $ state node]
        visited = S.empty
        dfs = \ o res v-> case o of
                            [] -> reverse res
                            (actual:rest) ->
                                dfs ([n | n <- expand actual,
                                            S.notMember (state n) v,
                                            find (== state n) (foldl (\acc node -> acc ++ [(state node)]) [] rest) == Nothing,
                                            (depth n) <= limit]
                                        ++ rest)
                                    (actual : res)
                                    (S.insert (state actual) v)
    in
        dfs open [] visited

iterativeDeepening :: (ProblemState s a, Ord s)
    => s
    -> Bool
    -> (Node s a, Int)
iterativeDeepening = \start eur ->
    let open = [(initial start)]
        initial = \ st -> (Node st Nothing Nothing 0)
        expand = \ node -> [ ( Node (snd x)
                                    (Just $ fst x)
                                    (Just node)
                                    ((depth node) + 1) ) | x <- if eur then sortBy (\n1 n2 -> compare (heuristic $ snd n1) (heuristic $ snd n2)) $ successors $ state node
                                                                        else successors $ state node]
        visited = S.empty
        dfs = \ o v limit -> case o of
                            [] -> (NilNode, length v)
                            (actual:rest) ->
                                if isGoal $ state actual then (actual, length v) else
                                dfs ([n | n <- expand actual,
                                            S.notMember (state n) v,
                                            find (== state n) (foldl (\acc node -> acc ++ [(state node)]) [] rest) == Nothing,
                                            (depth n) <= limit]
                                        ++ rest)
                                    (S.insert (state actual) v)
                                    limit

        deepening = \contor limit ->
            let result = dfs open visited limit
            in
                case result of
                    (NilNode, l) -> deepening (l + contor) (limit + 1)
                    (final, l)   ->  (final, l + contor)
    in
        deepening 0 0

extractPath :: Node s a -> [(a, s)]
extractPath = \node -> case previous node of
                            Nothing -> []
                            Just n -> (extractPath n) ++ [(fromJust $ action node, state node)]

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
