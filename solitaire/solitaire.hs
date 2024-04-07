import System.IO
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Control.Monad

{-# LANGUAGE ForeignFunctionInterface #-}
import Data.Char
import Foreign.C.Types
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
    c_getch :: IO CInt


data Suit = Diamonds | Clubs | Hearts | Spades deriving (Show)
type Card = (Int, Suit)
type Board = ([Card], [Card], [[Card]], [[Card]]) -- (deck, waste, stacks, foundations)


-- main :: IO ()
-- main = do
--     hSetBuffering stdin NoBuffering 
--     key <- getHiddenChar
--     putStrLn ("Inputted " ++ show key)


run :: a -> Int -> [a]
run x n = take n (repeat x)

random :: (RealFrac a, Floating a) => a -> a
random seed =
    let fract x = x - fromIntegral (floor x)
    in fract(sin(78.233 * seed) * 43758.5453)


resetConsole = putStr "\ESC[0m"

consoleColTest = do
    mapM_ (\x -> putStrLn ("\ESC[" ++ show x ++ "mtest" ++ show x ++ "\ESC[0m")) [30..107]--([40..47] ++ [100..107])
    resetConsole



cardWidth = 13
cardHeight = 10

boardWidth = 134
boardHeight = 40


icon :: Suit -> [String]
icon Diamonds =
    [
        "   █   ",
        "  ███  ",
        " █████ ",
        " █████ ",
        "  ███  ",
        "   █   "
    ]
icon Clubs = 
    [
        "  ▄█▄  ",
        " █████ ",
        " ▄███▄ ",
        "███████",
        "▀█████▀",
        "  ▄█▄  "
    ]
icon Hearts =
    [
        "▄██▄██▄",
        "███████",
        "███████",
        " █████ ",
        "  ███  ",
        "   █   "
    ]
icon Spades = 
    [
        "   █   ",
        "  ███  ",
        "▄█████▄",
        "███████",
        "▀█████▀",
        "  ▄█▄  "
    ]

suitChar :: Suit -> Char
suitChar Diamonds = '♦'
suitChar Clubs = '♣'
suitChar Hearts = '♥'
suitChar Spades = '♠'

suitCol :: Suit -> String
suitCol Diamonds = "\ESC[107m\ESC[91m"
suitCol Clubs = "\ESC[107m\ESC[30m"
suitCol Hearts = "\ESC[107m\ESC[91m"
suitCol Spades = "\ESC[107m\ESC[30m"

numStr :: Int -> (String, String)
numStr 1  = ("A ", " A")
numStr 2  = ("2 ", " 2")
numStr 3  = ("3 ", " 3")
numStr 4  = ("4 ", " 4")
numStr 5  = ("5 ", " 5")
numStr 6  = ("6 ", " 6")
numStr 7  = ("7 ", " 7")
numStr 8  = ("8 ", " 8")
numStr 9  = ("9 ", " 9")
numStr 10 = ("10", "10")
numStr 11 = ("J ", " J")
numStr 12 = ("Q ", " Q")
numStr 13 = ("K ", " K")

cardBackImage :: Bool -> [String] 
cardBackImage showCursor =
    let borderCol = if showCursor then "\ESC[106m" else "" 
    in
        [
            borderCol ++ "┌───────────┐\ESC[0m",
            borderCol ++ "│\ESC[104m           \ESC[0m" ++ borderCol ++ "│\ESC[0m",
            borderCol ++ "│\ESC[104m X X X X X \ESC[0m" ++ borderCol ++ "│\ESC[0m",
            borderCol ++ "│\ESC[104m  X X X X  \ESC[0m" ++ borderCol ++ "│\ESC[0m",
            borderCol ++ "│\ESC[104m X X X X X \ESC[0m" ++ borderCol ++ "│\ESC[0m",
            borderCol ++ "│\ESC[104m  X X X X  \ESC[0m" ++ borderCol ++ "│\ESC[0m",
            borderCol ++ "│\ESC[104m X X X X X \ESC[0m" ++ borderCol ++ "│\ESC[0m",
            borderCol ++ "│\ESC[104m  X X X X  \ESC[0m" ++ borderCol ++ "│\ESC[0m",
            borderCol ++ "│\ESC[104m           \ESC[0m" ++ borderCol ++ "│\ESC[0m",
            borderCol ++ "└───────────┘\ESC[0m"
        ]



-- negative card number for face-down cards
cardImage :: Bool -> Card -> [String]
cardImage showCursor (number, suit)
        | number < 0 = cardBackImage showCursor
        | True =
            let 
                borderCol = if showCursor then "\ESC[106m" else ""

                col = suitCol suit
                getLine iconLine = borderCol ++ "│" ++ col ++ "  " ++ iconLine ++ "  \ESC[0m" ++ borderCol ++ "│\ESC[0m" 
                (numStr1, numStr2) = numStr number

            in  [borderCol ++ "┌───────────┐\ESC[0m",
                 borderCol ++ "│" ++ col ++ numStr1 ++ "         \ESC[0m" ++ borderCol ++ "│\ESC[0m"]
                 ++ map getLine (icon suit) ++
                [borderCol ++ "│" ++ col ++ "         " ++ numStr2 ++ "\ESC[0m" ++ borderCol ++ "│\ESC[0m",
                 borderCol ++ "└───────────┘\ESC[0m"]


emptyFoundationImage :: Bool -> Suit -> [String]
emptyFoundationImage showCursor suit =
    let 
        borderCol = if showCursor then "\ESC[106m" else ""
        iconLines = map (\l -> borderCol ++ "│\ESC[0m\ESC[90m  " ++ l ++ "  \ESC[0m" ++ borderCol ++ "│\ESC[0m" ) (icon suit)
    in 
        [borderCol ++ "┌───────────┐\ESC[0m", 
         borderCol ++ "│\ESC[0m           " ++ borderCol ++ "│\ESC[0m"] 
         ++ iconLines ++ 
        [borderCol ++ "│\ESC[0m           " ++ borderCol ++ "│\ESC[0m", 
         borderCol ++ "└───────────┘\ESC[0m"]

genDeck :: [Card]
genDeck = foldr1 (++) (map (\suit -> map (\rank -> (rank, suit)) [1..13]) [Diamonds, Clubs, Hearts, Spades])

shuffle :: Double -> [a] -> [a]
shuffle _ [] = []
shuffle seed xs = 
    let
        index = floor $ (random seed) * fromIntegral (length xs)
        this = xs !! index
    in this : shuffle (seed + 1) (take index xs ++ drop (index + 1) xs) 

countNonColChars :: String -> Int
countNonColChars [] = 0
countNonColChars ('\ESC':xs) = countNonColChars (tail $ dropWhile (/= 'm') xs)
countNonColChars (_:xs) = 1 + countNonColChars xs

dropNonColChars :: Int -> String -> String
dropNonColChars _ [] = []
dropNonColChars 0 xs = xs
dropNonColChars n ('\ESC':xs) = dropNonColChars n (tail $ dropWhile (/= 'm') xs) -- remove all chars up to, and including, the next 'm' (but don't decrease the count)
dropNonColChars n (x:xs) = dropNonColChars (n-1) xs
        

takeNonColChars :: Int -> String -> String    -- does take the colour chars but doesn't count them as part of the count
takeNonColChars _ [] = []
takeNonColChars 0 _ = []
takeNonColChars n ('\ESC':xs) =
    let colStr = '\ESC' : takeWhile (/= 'm') xs ++ "m"
    in colStr ++ takeNonColChars n (drop (length colStr - 1) xs)
takeNonColChars n (x:xs) = x : takeNonColChars (n-1) xs

-- overlayImageX :: Int -> [String] -> [String] -> [String] -- clips bottom image if the something or other bottom image is bigger and extends past the top image, probably
-- overlayImageX x imgBottom imgTop = map (\(l1, l2) ->  takeNonColChars x l1 ++ l2) (zip imgBottom imgTop)

-- overlayImageY :: Int -> [String] -> [String] -> [String]
-- overlayImageY y imgBottom imgTop = take y imgBottom ++ imgTop


padImage :: Char -> (Int, Int) -> [String] -> [String]
padImage c (w, h) img = 
    let 
        tyroned str = str ++ run c (max (w - length str) 0) 
        tyrone = map tyroned img -- no sleep for second day in row. cannot think of better variable name
    in tyrone ++ run (run c w) (max (h - length tyrone) 0)


appendRightImage :: Int -> [String] -> [String] -> [String]
appendRightImage gap img1 img2 = 
    let paddedImage1 = padImage ' ' (foldr (max.countNonColChars) 0 img1, max (length img1) (length img2)) img1
    in map (\(x, y) -> x ++ run ' ' gap ++ y) (zip paddedImage1 img2)

appendBelowImage :: Int -> [String] -> [String] -> [String]
appendBelowImage gap img1 img2 = 
    let 
        width = max (foldr (max.countNonColChars) 0 img1) (foldr (max.countNonColChars) 0 img2)
        paddedImage1 = padImage ' ' (width, length img1) img1
        paddedImage2 = padImage ' ' (width, length img2) img2
    in paddedImage1 ++ run (run ' ' width) gap ++ paddedImage2


getStacksFromDeck :: Int -> [a] -> [[a]] -- head of return value is the remaining items in deck; tail is the list of stacks
getStacksFromDeck 0 deck = [deck]  
getStacksFromDeck n deck = getStacksFromDeck (n-1) (drop n deck) ++ [take n deck]


cardStackImage :: Int -> [Card] -> [String]
cardStackImage cursorY [] = []
cardStackImage cursorY [c] = cardImage (cursorY == 0) c
cardStackImage cursorY cards =
    let 
        -- -- whoever wrote this code is a moron
        -- l1             showCursor = "\ESC[0m┌───────────┐"
        -- l2 (num, suit) showCursor = if num >= 0 
        --                             then "\ESC[0m│" ++ suitCol suit ++ fst (numStr num) ++ "         \ESC[0m│"
        --                             else cardBackImage False !! 1
    -- in foldl1 (++) (map (\c -> [l1 False, l2 c False]) (reverse . tail $ cards)) ++ cardImage (head cards)

        showCursorBools = map (== cursorY) [0..(length cards - 1)]
    in 
        foldl1 
            (++) 
            ( map (\(b, c) -> take 2 (cardImage b c)) (zip showCursorBools (reverse . tail $ cards)) )
        ++ cardImage (last showCursorBools) (head cards)
    

deckImage :: Bool -> [Card] -> [String]
deckImage _ [] = 
    let row = "\ESC[0m" ++ run ' ' cardWidth
    in run row cardHeight
deckImage showCursor cards = cardBackImage showCursor

wasteImage :: Bool -> [Card] -> [String]
wasteImage showCursor cards = cardStackImage (if showCursor then 2 else (-1)) (take 3 cards)

stacksImage :: (Int, Int) -> [[Card]] -> [String]
stacksImage (cursorX, cursorY) stacks = 
    let 
        stacksContainCursor = map (==cursorX) [1..(length stacks)]
        imgs = map (\(b, s) -> cardStackImage (if b then cursorY else (-1)) s) (zip stacksContainCursor stacks)
    in foldr1 (appendRightImage 1) imgs  



foundationsImage :: Int -> [[Card]] -> [String]
foundationsImage cursorY founds = 
    let 
        image (f, suit, yPos) = case f of
                               [] -> emptyFoundationImage (yPos == cursorY) suit
                               (x:_) -> cardImage (yPos == cursorY) x
        fImages = map image (zip3 founds [Diamonds, Clubs, Hearts, Spades] [0..3])
    in foldr (appendBelowImage 0) [] fImages


showBoard :: (Int, Int) -> Board -> IO () 
showBoard (cX, cY) (deck, waste, stacks, foundations) = do
    let 
        deckAndWaste = appendBelowImage 0 ( deckImage ((cX, cY) == (0, 0)) deck) (wasteImage ((cX, cY) == (0, 1)) waste)  
        screen = foldr1 (appendRightImage 5)  [deckAndWaste, stacksImage (cX, cY) stacks, foundationsImage (if cX == 8 then cY else (-1)) foundations] 

    mapM_ putStrLn screen


-- inputTurn :: (Int, Int) -> Board -> IO ()
inputTurn (cX, cY) (deck, waste, stacks, foundations) = do
    showBoard (cX, cY) (deck, waste, stacks, foundations)

    key <- getHiddenChar
    putStrLn (show key)

    let newCursor = case key of 
                'w' -> (cX, cY - 1)
                'a' -> (cX - 1, cY)
                's' -> (cX, cY + 1)
                'd' -> (cX + 1, cY)
                _   -> (cX, cY)
    
    
        loopCursorHorizontal (x, y) = (x `mod` 9, y)

        loopCursorStackVertical y stack =  
            let minFaceUpCard = length . filter (\(n, _) -> n < 0) $ stack
            in (y - minFaceUpCard) `mod` (length stack - minFaceUpCard) + minFaceUpCard

        loopCursorVertical (x, y) = case () of _
                                                 | x == 0         -> (x, y `mod` 2)
                                                 | x > 0 && x < 8 -> (x, loopCursorStackVertical y (stacks !! (x - 1)))
                                                 | x == 8         -> (x, y `mod` 4)
                                                 | True           -> (x, y)


        adjustedNewCursor = loopCursorVertical . loopCursorHorizontal $ newCursor

    if key /= '\ESC' then inputTurn adjustedNewCursor (deck, waste, stacks, foundations) else return ()


main = do
    hSetBuffering stdin NoBuffering 
    time <- getCurrentTime

    cursor <- return (0, 0)
    
    let 
        seed = (realToFrac . utcTimeToPOSIXSeconds $ time) :: Double
        startingDeck = shuffle seed genDeck
        (deck:stacksFaceUp) = getStacksFromDeck 7 startingDeck
        stacks = map (  \(h:t) -> h : map (\(num, suit) -> (-num, suit)) t  ) stacksFaceUp

        stacksTest = [(1, Hearts), (2, Hearts), (3, Hearts), (4, Hearts), (5, Hearts), ((-6), Hearts)] : tail stacks

        board = (deck, [(3, Hearts), (3, Clubs), (3, Hearts), (3, Clubs)], stacksTest, run [] 4) :: Board
        -- board = (deck, [(3, Hearts), (3, Clubs), (3, Hearts), (3, Clubs)], stacks, [[(1, Hearts)], [], [], []]) :: Board

    inputTurn cursor board


-- test y stack =  
--     let minFaceUpCard = (length . filter (\(n, _) -> n < 0) $ stack)
--     in max minFaceUpCard (y `mod` (length stack))