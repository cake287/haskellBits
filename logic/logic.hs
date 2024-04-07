import Data.Char
import Data.List

help = putStrLn "To generate a table, call: table <expression>\nFor example: table \"A.B | C\"\nUse any of the following to create an expression:\n- Numbers 0 and 1\n- Letters A-Z\n- Operators + | . -> = !"


isVar :: Char -> Bool
isVar c = let n = ord c in (n >= 97 && n <= 122) || (n >= 65 && n <= 90)

isOp :: Char -> Bool
isOp c = c `elem` "+|.&>="

isUnaryOp :: Char -> Bool
isUnaryOp c = c `elem` "!¬~"

leftAssociative op = op `elem` "+|.&"

precedence '=' = 0
precedence '>' = 1
precedence '+' = 2
precedence '|' = 2
precedence '.' = 3
precedence '&' = 3
precedence other = error ("operator \'" ++ [other] ++ "\' precedence not found")

popOpStack :: Char -> [Char] -> [Char]
popOpStack o1 opStack = takeWhile (\o2 -> isOp o2 && ((leftAssociative o1 && precedence o1 <= precedence o2) || (not (leftAssociative o1) && precedence o1 < precedence o2))) opStack

-- shunting yard algorithm
infixToPostfix :: [Char] -> [Char] -> [Char]
infixToPostfix [] [] = []
infixToPostfix [] (opX:opXs)
        | opX == '(' = error "mismatched brackets"
        | True = opX : (infixToPostfix [] opXs)
infixToPostfix (x:xs) opStack
        | x == '0' || x == '1' = x : infixToPostfix xs opStack
        | isVar x = x : infixToPostfix xs opStack
        | isOp x = 
            let 
                additionalOutput = popOpStack x opStack
                newOpStack = x : (drop (length additionalOutput) opStack)
            in 
                additionalOutput ++ (infixToPostfix xs newOpStack)
        | isUnaryOp x = infixToPostfix xs (x:opStack)
        | x == '(' = infixToPostfix xs (x:opStack)
        | x == ')' = 
            let
                additionalOutput = takeWhile (/= '(') opStack
                newOpStack = drop (length additionalOutput + 1) opStack -- length + 1 to remove the left bracket (but not append it to the output)
            in
                if additionalOutput == opStack 
                then error "mismatched brackets" 
                else 
                    if (length newOpStack > 0 && isUnaryOp (head newOpStack))
                    then additionalOutput ++ [head newOpStack] ++ (infixToPostfix xs (tail newOpStack))
                    else additionalOutput ++ (infixToPostfix xs newOpStack)
        | True = error "unknown token \'" ++ [x] ++ "\' found"


getVariables :: [Char] -> [Char]
getVariables str = map (chr . head) . group . sort $ map ord (filter isVar str)


replaceVar:: [(Char, Char)] -> Char -> Char
replaceVar dict token
    | length matchingPairs == 0 = error ("value for variable \'" ++ [token] ++ "\' not supplied")
    | True = snd . head $ matchingPairs
    where matchingPairs = filter (\(a, b) -> a == token) dict

replaceVarsWithVals exp dict = map (\ x -> if isVar x then replaceVar dict x else x) exp


not' :: Char -> Bool
not' x = x == '0'

or' :: Char -> Char -> Bool
or' x y = x /= '0' || y /= '0'

and' :: Char -> Char -> Bool
and' x y = x /= '0' && y /= '0'

implies :: Char -> Char -> Bool
implies x y = x /= '0' || not (y /= '0')

equals :: Char -> Char -> Bool
equals x y = (x /= '0') == (y /= '0')

boolToChar :: Bool -> Char
boolToChar True = '1'
boolToChar False = '0'

eval :: [Char] -> [Char] -> Char
eval [s] [] = s
eval (s1:stack) ('!':xs) = eval (boolToChar (not' s1) : stack) xs 
eval (s1:stack) ('¬':xs) = eval (boolToChar (not' s1) : stack) xs 
eval (s1:stack) ('~':xs) = eval (boolToChar (not' s1) : stack) xs 
eval (s1:s2:stack) ('+':xs) = eval (boolToChar (or' s1 s2) : stack) xs
eval (s1:s2:stack) ('|':xs) = eval (boolToChar (or' s1 s2) : stack) xs
eval (s1:s2:stack) ('.':xs) = eval (boolToChar (and' s1 s2) : stack) xs
eval (s1:s2:stack) ('&':xs) = eval (boolToChar (and' s1 s2) : stack) xs
eval (s1:s2:stack) ('>':xs) = eval (boolToChar (implies s1 s2) : stack) xs
eval (s1:s2:stack) ('=':xs) = eval (boolToChar (equals s1 s2) : stack) xs
eval stack (x:xs) = eval (x:stack) xs


decToBin :: Int -> Int -> [Char]
decToBin (-1) n = ""
decToBin exp n 
        | n - pow >= 0 = '1': decToBin (exp-1) (n-pow) 
        | True = '0' : decToBin (exp-1) n 
        where pow = 2^exp

-- incrementBin :: [Char] -> [Char]
-- incrementBin ['1'] = "X" -- mark that the string has overflowed
-- incrementBin str
--         | digit == '0' = init str ++ "1"
--         | True = incrementBin (init str) ++ "0" 
--         where digit = last str

spaceStr :: [Char] -> [Char]
spaceStr str = foldr (\ x acc -> [x] ++ " " ++ acc) "" str

repeatChar :: Char -> Int -> [Char]
repeatChar c n = take n (repeat c)

getRows :: [Char] -> [[Char]]
getRows str =
    let 
        str2 = filter (\ x -> x /= ' ' && x /= '-') str -- remove '-' so that "->" becomes a single character, ">"
        postfixExp = infixToPostfix str2 ""
        vars = getVariables str2
        vals = map (decToBin (length vars - 1))  [0..(2^(length vars)-1)]
        varDicts = map (zip vars) vals
        exps = map (replaceVarsWithVals postfixExp) varDicts
        results = map (eval "") exps
        spacedVals = map spaceStr vals

        row1 = spaceStr vars ++ "| " ++ str 
        row2 = repeatChar '-' (2* length vars) ++ "|-" ++ repeatChar '-' (length str + 1)
        rows = zipWith (\ x y -> x ++ "| " ++ repeatChar ' ' ((length str) `div` 2) ++ [y]) spacedVals results
    in
        row1 : row2 : rows

table str = mapM_ putStrLn (getRows str)