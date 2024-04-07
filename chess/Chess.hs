import Data.Char
import Data.List

-- (board, (can white castle, can black castle, last double pawn move))
-- can castle = (can castle queen side, can castle king side)
-- last double pawn move -> the file of the pawn if the last move was a double pawn move (0 based). otherwise -1. for en passant
type Board = ([String], ((Bool, Bool), (Bool, Bool), Int)) 

boardInit :: Board
boardInit = ([
    "RNBKQBNR", 
    "PPPPPPPP",
    "--------",
    "--------",
    "--------",
    "--------",
    "pppppppp",
    "rnbkqbnr"

    -- "R---K--R",
    -- "--------",
    -- "--------",
    -- "--------",
    -- "--------",
    -- "--------",
    -- "-P------",
    -- "r--k---r"
    ],
    ((True, True), (True, True), -1)
    )

getAt2DArr :: [[a]] -> (Int, Int) -> a
getAt2DArr arr (x, y) = (arr !! y) !! x

getAt :: Board -> (Int, Int) -> Char
getAt (board, _) pos = getAt2DArr board pos

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after
setAt :: [String] -> (Int, Int) -> Char -> [String]
setAt board (x, y) c = 
    let newLine = modify_list (board !! y) x c
    in modify_list board y newLine


getPieceType :: Board -> (Int, Int) -> Char 
getPieceType board pos = toUpper . getAt board $ pos

isPieceAt :: Board -> Char -> (Int, Int) -> Bool
isPieceAt board piece pos = getAt board pos == piece


movePiece :: [String] -> (Int, Int) -> (Int, Int) -> [String]
movePiece board piecePos targetPos = 
    let newBoard = setAt board targetPos (getAt2DArr board piecePos)
    in if piecePos /= targetPos then setAt newBoard piecePos '-' else board

updateBoard :: Board -> (Int, Int) -> (Int, Int) -> Board
updateBoard (board, info) (pX, pY) (tX, tY0) = 
    let tY = tY0 `mod` 10
        pawnPromCode = tY0 `div` 10
        
        col = getPieceCol (board, info) (pX, pY)
        pType = getPieceType (board, info) (pX, pY)
        (whiteCastleInfo, blackCastleInfo, enPassantFile) = info
        
        newBoard = movePiece board (pX, pY) (tX, tY)

        newBoard2 = if pawnPromCode == 0 then newBoard -- could remove the if statement and add a 0 case, which evaluates to pType. might be slightly cleaner code. would perform worse although it doesn't matter. it doesn't matter. it's 3:40 am. 
                    else setAt newBoard (tX, tY) . convertPieceToCol col $ case pawnPromCode of
                        1 -> 'N'
                        2 -> 'B'
                        3 -> 'R'
                        4 -> 'Q'
                        _ -> error "invalid pawn promotion code" 


        takenEnPassant = (tX, tY) == (enPassantFile, (if col == White then 5 else 2)) && pType == 'P' 
        newBoard3 = if takenEnPassant then setAt newBoard2 (tX, tY - pawnDir col) '-' else newBoard2 -- if en passant was taken, then remove the taken pawn
    
        newEnPassantFile = if abs (pY - tY) == 2 && pType == 'P' then tX else -1


        (cq, ck) = if col == White then whiteCastleInfo else blackCastleInfo -- cq = canQueenSideCastle, ck = canKingSideCastle 
        newCastlingInfo =   if pType == 'K' then (False, False) 
                            else if pType == 'R' then (
                                if (pX, pY) == (0, 0) && col == White || (pX, pY) == (0, 7) && col == Black then (False, ck)
                                else if (pX, pY) == (7, 0) && col == White || (pX, pY) == (7, 7) && col == Black then (cq, False)
                                else (cq, ck)
                                )
                            else (cq, ck)
        
        -- -1 for queen side, 1 for king side, 0 for not castled
        -- n.b. must include "abs (tX - pX) == 2" since (-1) `div` 2 evaluates to -1 instead of 0 
        castleDir = if pType == 'K' && abs (tX - pX) == 2 then (tX - pX) `div` 2 else 0 
        newBoard4 = if castleDir /= 0 
                    then movePiece newBoard3 ((if castleDir == 1 then 7 else 0), pY) (pX + castleDir, tY) 
                    else newBoard3

    in (newBoard4, 
        (if col == White then newCastlingInfo else whiteCastleInfo, 
         if col == Black then newCastlingInfo else blackCastleInfo, 
        newEnPassantFile))


data Col = White | Black | None deriving(Show, Read, Eq)

getPieceCol :: Board -> (Int, Int) -> Col
getPieceCol board pos
        | c == '-' = None
        | toLower c == c = Black
        | True = White
        where c = getAt board pos

convertPieceToCol :: Col -> Char -> Char
convertPieceToCol White c = toUpper c
convertPieceToCol Black c = toLower c 

oppositeCol :: Col -> Col
oppositeCol White = Black
oppositeCol Black = White

-- n.b. you can't just use (a /= b) since we need to return false if either argument is None (i.e. empty square)
areColoursOpposite :: Col -> Col -> Bool
areColoursOpposite a b = (a == White && b == Black) || (a == Black && b == White)

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

findPieces :: Board -> Char -> [(Int, Int)]
findPieces (board, _) piece =
    let xs = map (\row -> piece `elemIndices` row) board
        coords = map (\y -> map (\x -> (x, y)) (xs !! y)) [0..(length board - 1)]
    in concat coords

findPiecesOfCol :: Board -> Col -> [(Int, Int)]
findPiecesOfCol (board, _) col =
    let pred c = (c /= '-') && (if col == White then c == toUpper c else c == toLower c)
        xs = map (\row -> findIndices pred row) board
        coords = map (\y -> map (\x -> (x, y)) (xs !! y)) [0..(length board - 1)]
    in concat coords

moveDirFuncs :: Char -> [(Int, Int) -> (Int, Int)]
moveDirFuncs 'B' = [(\(x, y) -> (x+1, y+1)), (\(x, y) -> (x-1, y+1)), (\(x, y) -> (x+1, y-1)), (\(x, y) -> (x-1, y-1))]
moveDirFuncs 'R' = [(\(x, y) -> (x+1, y)),   (\(x, y) -> (x-1, y)),   (\(x, y) -> (x, y+1)),   (\(x, y) -> (x, y-1))]
moveDirFuncs 'Q' = moveDirFuncs 'B' ++ moveDirFuncs 'R'

findMovesInDir :: Board -> (Int, Int) -> Col -> ((Int, Int) -> (Int, Int)) -> [(Int, Int)]
findMovesInDir board (x, y) pCol f = -- f is the direction function
    let 
        moves = takeWhile (\pos -> inBounds pos && isPieceAt board '-' pos) . tail . iterate f $ (x, y) 
        extraMove = filter (\pos -> inBounds pos && areColoursOpposite pCol (getPieceCol board pos)) (if moves /= [] then [f (last moves)] else [f (x, y)]) -- if this direction found any moves, check another move in that dir after the last for a piece to take. if there were no moves found, check one move after the piece start pos.
    in moves ++ extraMove

knightMoves :: (Int, Int) -> [(Int, Int)]
knightMoves (x, y) = [(x+1, y+2), (x-1, y+2), (x+1, y-2), (x-1, y-2), (x+2, y+1), (x-2, y+1), (x+2, y-1), (x-2, y-1)] 

pawnDir :: Col -> Int
pawnDir White = 1
pawnDir Black = -1

-- get all moves without considering if they will put your own king in check. this is used in isInCheck so that a piece that is pinned can still deliver check
getPossibleMoves :: Board -> (Int, Int) -> [(Int, Int)]
getPossibleMoves board (x, y)
        | pType =='P' = 
            let
                dir = pawnDir col

                move1 = filter (isPieceAt board '-') [(x, y + dir)] -- moving forward 1
                move2 = filter (\pos-> move1 /= [] && (y==1 && col==White || y==6 && col==Black) && isPieceAt board '-' pos) [(x, y + 2*dir)] -- moving forward 2 at start
                moves3 = filter (\pos -> inBounds pos && getPieceCol board pos /= col && not (isPieceAt board '-' pos)) [(x-1, y+dir), (x+1, y+dir)] -- capturing enemy piece
                
                enPassantFile = (\(_, (_, _, f)) -> f) board
                move4 = filter (\(x2, y2) -> inBounds (x2, y2) && x2 == enPassantFile && y2 == (if col == White then 5 else 2)) [(x-1, y+dir), (x+1, y+dir)]
            in
                move1 ++ move2 ++ moves3 ++ move4
        | pType =='N' = filter (\pos -> inBounds pos && getPieceCol board pos /= col) $ knightMoves (x, y)
        | pType =='B' || pType == 'R' || pType == 'Q' = concat $ map (findMovesInDir board (x, y) col) (moveDirFuncs pType)
        | pType =='K' = -- filter (\pos -> inBounds pos && getPieceCol board pos /= col) [(x+1, y), (x-1, y), (x, y+1), (x, y-1), (x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1, y-1)]
            let singleMoves = filter (\pos -> inBounds pos && getPieceCol board pos /= col) [(x+1, y), (x-1, y), (x, y+1), (x, y-1), (x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1, y-1)]
                
                (canQueenSideCastle, canKingSideCastle) = (\(_, (w, b, _)) -> if col == White then w else b) board
                castlingMoves = filter -- i hate castling
                    (\(steps, canCastle) -> 
                        canCastle && 
                        (foldr1 (&&) $ map ((=='-'). getAt board) steps)
                    )
                    [([(x+1, y), (x+2, y)], canKingSideCastle), ([(x-1, y), (x-2, y), (x-3, y)], canQueenSideCastle)]
            in singleMoves ++ map ( \ ((_:m:_), _) -> m) castlingMoves
        where pType = getPieceType board (x, y)
              col = getPieceCol board (x, y)

-- check all knight, queen, rook, bishop and attacking pawn moves away from the king
isInCheck :: Col -> Board -> Bool
isInCheck col board = 
    let kingLocations = findPieces board $ convertPieceToCol col 'K'
        (kX, kY) = if length kingLocations == 1 then head kingLocations else error $ "not exactly one king of colour " ++ show col  -- king pos
        test p1 p2 = p2 == convertPieceToCol (oppositeCol col) p1

        pawnMovesAway = filter inBounds [(kX + 1, kY + pawnDir col), (kX - 1, kY + pawnDir col)]
        enemyPawns = [] /= (filter (test 'P') . map (getAt board) $ pawnMovesAway)

        knightMovesAway = filter inBounds . knightMoves $ (kX, kY)
        enemyKnights = [] /= (filter (test 'N') . map (getAt board) $ knightMovesAway)

        firstPieceInDir dirFunc = 
            let pos = head . dropWhile (\pos -> inBounds pos && isPieceAt board '-' pos) . tail . iterate dirFunc $ (kX, kY)
            in if inBounds pos then getAt board pos else '-' 
        
        otherEnemies = [] /= (concat $ map (\p -> filter (\p2 -> test p p2 || test 'Q' p2) . map firstPieceInDir $ moveDirFuncs p) "BR")
    in enemyPawns || enemyKnights || otherEnemies

isMoveSafe :: Board -> Col -> (Int, Int) -> (Int, Int) -> Bool
isMoveSafe board col (pX, pY) (tX, tY) 
        | getPieceType board (pX, pY) == 'K' && abs (pX - tX) == 2 = 
            let stepMoves = [(x, pY) | x <- [pX, pX + (tX - pX) `div` 2 .. tX]]
            in foldr1 (&&) $ map test stepMoves
        | True = test (tX, tY) 
        where test t = not . isInCheck col $ updateBoard board (pX, pY) t

-- get moves of the piece at pos, that do not put your own king in check
getSafeMoves :: Board -> Col -> (Int, Int) -> [(Int, Int)]
getSafeMoves board colTurn pos =
    let possibleMoves = getPossibleMoves board pos
        safeMoves = filter (isMoveSafe board colTurn pos) possibleMoves
    in safeMoves -- ++ if getPieceType board pos == 'K' then castlingMoves board colTurn pos safeMoves else []

data MateType = NotMate | Stalemate | Checkmate deriving(Show, Read, Eq)
isMate :: Col -> Board -> MateType
isMate colTurn board 
        | possibleMoves /= [] = NotMate
        | isInCheck colTurn board = Checkmate
        | True = Stalemate
        where possibleMoves = concat $ map (getSafeMoves board colTurn) (findPiecesOfCol board colTurn)



printBoard :: Board -> IO ()
printBoard (board, info) = mapM_ putStrLn (reverse board) -- >> print info

-- assumes it's the turn of the piece on the given square - this function is only for debugging.
showPossibleMoves :: (Int, Int) -> IO ()
showPossibleMoves pos = printBoard 
    ( (foldr (\p acc -> setAt acc p '#') (fst boardInit) (getSafeMoves boardInit (getPieceCol boardInit pos) pos)),
    snd boardInit)

notationToCoord :: String -> (Int, Int)
notationToCoord [c1, c2] = (ord c1 - ord 'a', ord c2 - ord '1')

-- input should be in format "e3f4"
goodInput :: Board -> Col -> String -> Bool
goodInput board colTurn [c1, c2, c3, c4] =
    let charTest c = ord c >= ord 'a' && ord c <= ord 'h'
        numTest c = ord c >= ord '1' && ord c <= ord '8'
        goodFormat = charTest c1 && numTest c2 && charTest c3 && numTest c4

        piecePos = notationToCoord [c1, c2]
        targetPos = notationToCoord [c3, c4]
        coordsInBounds = if goodFormat then inBounds piecePos && inBounds targetPos else False

    in if coordsInBounds && getPieceCol board piecePos == colTurn then targetPos `elem` (getSafeMoves board colTurn piecePos) else False
goodInput _ _ _ = False


pawnPromInput :: IO Int
pawnPromInput = do
    putStr "Choose piece for pawn promotion (k[N]ight, [B]ishop, [R]ook or [Q]ueen): "
    input <- getLine
    case (map toUpper input) of
        "N"      -> return 1
        "K"      -> return 1
        "KNIGHT" -> return 1
        "B"      -> return 2
        "BISHOP" -> return 2
        "R"      -> return 3
        "ROOK"   -> return 3
        "Q"      -> return 4
        "QUEEN"  -> return 4
        _ -> pawnPromInput

getInput :: Board -> Col -> IO ((Int, Int), (Int, Int))
getInput board colTurn = do
    input <- getLine
    if goodInput board colTurn input 
        then do let ((pX, pY), (tX, tY)) = ((notationToCoord (take 2 input)), notationToCoord (drop 2 input)) 
                pawnPromCode <- if getPieceType board (pX, pY) == 'P' && tY == (if getPieceCol board (pX, pY) == White then 7 else 0) 
                                then pawnPromInput 
                                else return 0 
                return ((pX, pY), (tX, pawnPromCode*10 + tY)) 
        else putStrLn "Move not possible" >> getInput board colTurn


nextTurn :: Board -> Col -> IO ()
nextTurn board colTurn = do

    printBoard board 
    putStrLn $ if isInCheck colTurn board then "\nCheck" else ""
    putStrLn ("Current turn: " ++ show colTurn) 

    (piecePos, targetPos) <- getInput board colTurn
    if (\(_, tY) -> tY >= 10) targetPos then putStrLn "pawn prom" else return ()

    let newBoard = updateBoard board piecePos targetPos
        mateType = isMate (oppositeCol colTurn) newBoard
    if mateType == NotMate
    then nextTurn newBoard (oppositeCol colTurn)
    else printBoard newBoard >> print mateType

main = nextTurn boardInit White
