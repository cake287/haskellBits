module AI where

import AnarchyChess

searchDepth = 2

inf :: Int
inf = maxBound :: Int

-- most ideas for heuristic eval function taken from https://www.dailychess.com/rival/programming/evaluation.php
evalSide :: Board -> Col -> Int
evalSide board col = let
    pawnPoses = findPieces board $ convertPieceToCol col 'P'
    pawnAdvanceBonuses = (*5) . sum $ map ( 
        (\(x, y) -> floor (3.5 - abs (fromIntegral x - 3.5)) + max 0 (y - 2)) .
        (\(x, y) -> (x, if col == White then y else 7 - y))
        ) 
        pawnPoses

    -- for each pawn on the same file as another pawn, deduct 4 points. leads to -8 points for double stacked, -24 points for triple stack
    pawnStackPenalty = (*4) . sum $ map (\(x1, y1) -> length . filter (\(x2, y2) -> x1 == x2 && y1 /= y2) $ pawnPoses) pawnPoses

    -- add isolated pawn penalty (-2)
    -- add passed pawn bonus 



    in 100 * length pawnPoses -- + pawnAdvanceBonuses - pawnStackPenalty

eval :: Board -> Col -> Int
eval board col = evalSide board col - evalSide board (oppositeCol col)

-- minimax search with alpha beta pruning
-- TODO - calling isMate computes all successor states. no need to call it again (to continue the search)
--        to remove the redundancy, could create a new isMate function which takes the list of succ states as an argument so doesn't need to compute them again
search2 :: Board -> Col -> Col -> (Int, Int) -> Int -> Int
search2 board startCol _   _      0 = eval board startCol
search2 board startCol col (a, b) depth
        | mateType == NotMate = 
            let maxPlayer = startCol == col
                succs = succStates board col

                compareFunc = if maxPlayer then max else min
                foldFunc succ (value, newCutoff) = 
                    if (if maxPlayer then value > b else value < a)
                    then (value, newCutoff)  -- cut off search
                    else 
                        let newVal = compareFunc value $ search2 succ startCol (oppositeCol col) (if maxPlayer then newCutoff else a, if not maxPlayer then newCutoff else b) (depth - 1)
                        in (
                            newVal,
                            compareFunc newVal newCutoff
                        ) 


            in fst $ foldr 
                foldFunc
                (if maxPlayer then (-inf, a) else (inf, b)) 
                succs  

        -- subtract the number of turns it would take to achieve this mate. this incentivises winning (or drawing) as quickly as possible, and losing as late as possible.
        | mateType == Stalemate = 0 -- not currently subtracting for stalemate -- (searchDepth - depth) * if startCol == col then -1 else 1 
        | mateType == Checkmate = (100000 - (searchDepth - depth)) * if startCol == col then -1 else 1 
        where mateType = isMate col board


search :: Board -> Col -> Board
search board col =
    let searchCall state = search2 state col col (-inf, inf) searchDepth
    -- in searchCall board
        (succHead : succsTail) = succStates board col

        foldFunc thisSucc (bestSucc, bestScore) =
            let thisScore = searchCall thisSucc
            in if thisScore > bestScore then (thisSucc, thisScore) else (bestSucc, bestScore) 
    in fst $ foldr foldFunc (succHead, searchCall succHead) succsTail
