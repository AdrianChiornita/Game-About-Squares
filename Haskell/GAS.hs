{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module GAS where

import qualified Data.Map.Strict as M
import           ProblemState

type Position = (Int, Int)

data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

data Object = Square { col :: Color
                     , hed :: Heading }
            | Circle { col :: Color }
            | Arrow { hed :: Heading }
    deriving (Eq, Ord)

instance Show Object where
    show (Square color heading) = case color of
                                    Red  -> "R" ++ show heading
                                    Blue -> "B" ++ show heading
                                    Gray -> "G" ++ show heading
    show (Circle color) = case color of
                            Red  -> "r"
                            Blue -> "b"
                            Gray -> "g"
    show (Arrow heading) = show heading

data Level = Level { gamemap :: M.Map Position [Object] }
    deriving (Eq, Ord)

instance Show Level where
    show lev = let s = size lev
                   minx = fst $ fst s
                   miny = snd $ fst s
                   maxx = fst $ snd s
                   maxy = snd $ snd s
                   delimiter = \pos -> if snd pos == maxy then (if fst pos /= maxx then "\n" else "") else "|"
                in
                    foldl (\prt pos -> case M.lookup pos (gamemap lev) of
                                            Nothing -> prt ++ "   " ++ delimiter pos
                                            Just objs -> prt ++ (if length objs == 1 then
                                                                    case head objs of
                                                                        Square c h -> show (Square c h) ++ " "
                                                                        Circle c -> "  " ++ show (Circle c)
                                                                        Arrow h -> "  " ++ show (Arrow h)
                                                                    else
                                                                        foldl (\acc obj -> acc ++ show obj) "" objs
                                                                    ) ++ delimiter pos
                          )
                          ""
                          [(x,y) | x <- [minx..maxx], y <- [miny..maxy]]

nextposition :: Position -> Heading -> Position
nextposition = \ (x,y) heading -> case heading of
                                    North -> (x - 1, y)
                                    East  -> (x, y + 1)
                                    South -> (x + 1, y)
                                    West  -> (x, y - 1)

size :: Level -> (Position,Position)
size = \lev -> M.foldlWithKey (\acc (x,y) _ -> let minx = min x $ fst $ fst acc
                                                   miny = min y $ snd $ fst acc
                                                   maxx = max x $ fst $ snd acc
                                                   maxy = max y $ snd $ snd acc
                                                in ((minx,miny),(maxx,maxy))
                                )
            ((maxBound,maxBound),(minBound,minBound)) $ gamemap lev

emptyLevel :: Level
emptyLevel = Level M.empty

addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare = \c h pos lev -> Level $ M.insertWith (++) pos [(Square c h)] $ gamemap lev

addCircle :: Color -> Position -> Level -> Level
addCircle = \c pos lev -> Level $ M.insertWith (++) pos [(Circle c)] $ gamemap lev

addArrow :: Heading -> Position -> Level -> Level
addArrow = \h pos lev -> Level $ M.insertWith (++) pos [(Arrow h)] $ gamemap lev


deleteObject :: Position -> Level -> Level
deleteObject = \ pos lev -> case M.lookup pos (gamemap lev) of
                                Nothing   -> lev
                                Just objs -> if length objs == 1 then
                                                Level $ M.delete pos $ gamemap lev
                                             else
                                                Level $ M.adjust tail pos $ gamemap lev

move :: Position
     -> Level
     -> Level
move = \pos lev -> case M.lookup pos (gamemap lev) of
                            Nothing   -> lev
                            Just objs -> case head objs of
                                            Circle _ -> lev
                                            Arrow _  -> lev
                                            Square c h -> let newpos = nextposition pos h
                                                              l = deleteObject pos lev
                                                              nl = case M.lookup newpos (gamemap l) of
                                                                        Nothing   -> addSquare c h newpos l
                                                                        Just list -> case head list of
                                                                                        Circle _   -> addSquare c h newpos l
                                                                                        Arrow ar   -> addSquare c ar newpos l
                                                                                        Square _ _ -> addSquare c h newpos (moverec newpos h l)
                                                            in nl

moverec :: Position -> Heading -> Level -> Level
moverec = \pos h lev -> case M.lookup pos (gamemap lev) of
                                Nothing   -> lev
                                Just objs -> case head objs of
                                                Circle _    -> lev
                                                Arrow _     -> lev
                                                Square c hd -> let newpos = nextposition pos h
                                                                   l = deleteObject pos lev
                                                                   nl = case M.lookup newpos (gamemap l) of
                                                                            Nothing   -> addSquare c hd newpos l
                                                                            Just list -> case head list of
                                                                                            Circle _   -> addSquare c hd newpos l
                                                                                            Arrow ar   -> addSquare c ar newpos l
                                                                                            Square _ _ -> addSquare c hd newpos (moverec newpos h l)
                                                                in nl

instance ProblemState Level Position where
    successors = \lev ->
        M.foldlWithKey (\acc (x,y) objs ->
                    case head objs of
                        Square _ _ -> acc ++ [ ((x,y), move (x,y) lev) ]
                        _          -> acc
            )
            []
            $ gamemap lev

    isGoal = \lev ->
        M.foldlWithKey (\acc _ objs ->
                    case head objs of
                        Square c _ -> case last objs of
                                            Circle cc -> if cc == c then acc && True
                                                                    else acc && False
                                            _ -> acc && False
                        Arrow _ -> acc && True
                        _ -> acc && False
            )
            True
            $ gamemap lev

    heuristic = \lev ->
        M.foldlWithKey (\acc (a,b) objs ->
            case head objs of
                Square cs _ ->
                    acc + (M.foldlWithKey (\acum (c,d) objts ->
                            acum + foldl (\ac e -> case e of
                                                    Circle cc -> if cc == cs then abs (a - c) + abs (b - d)
                                                                            else ac
                                                    _ -> ac
                                    )
                                    0
                                    objts
                        )
                        0
                        $ gamemap lev)
                _ -> acc
        )
        0
        $ gamemap lev
