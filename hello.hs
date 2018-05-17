-- http://learn.hfm.io/index.html
import Data.Char 

echo = putStrLn "Hello, World!"
mylength s = length (s)
mymax :: Ord a => a -> a -> a
mymax x y = if x >= y then x else y
t = mymax 3 4
addMul :: Num a => a -> a -> (a, a)
addMul x y = (x + y, x * y)
main2 = putStrLn(show(mylength("Hello " ++ "World!")))
main3 = putStrLn(show t )
(res,res2) = addMul 1 2
arr = [1,2,3] ++ [4,5]
main4 = putStrLn(show(tail(arr)))
mymax2 :: Ord a => a -> a -> a
mymax2 x y | x >= y = x
			| y > x = y
t2 = mymax 3 4
main5 = putStrLn(show t2) 

natSum :: (Num a, Ord a) => a -> a
natSum 0              = 0
natSum n  | n > 0     = n + natSum (n - 1) 
          | otherwise = error "natSum: Input value too small!"
main6 = putStrLn(show(natSum(3)))

repeatN :: Int -> a -> [a]
repeatN 0 x  = []
repeatN n x  = x : repeatN (n - 1) x
a = repeatN 3 5
main7 = putStrLn(show(a!!0))

suffixes :: String -> [String]
suffixes ""  = []
suffixes str = str : suffixes (tail str)
b = suffixes "TOto"
main8 = putStrLn(show(b!!2))

allSquares :: Num a => [a] -> [a]
allSquares []       = []
allSquares (x : xs) = x * x : allSquares xs
c = allSquares [1,2,3]
main9 = putStrLn(show(c!!2))



allToUpper :: String -> String
allToUpper []                 = []
allToUpper (chr : restString) = toUpper chr : allToUpper restString
d = allToUpper "tedt"
main10 = putStrLn(d)

extractDigits :: String -> String
extractDigits []
  = []
extractDigits (chr : restString)
  | isDigit chr = chr : extractDigits restString
  | otherwise   =       extractDigits restString
e = extractDigits "1tedt"  
main11 = putStrLn(show(e!!0))

myproduct :: Num a => [a] -> a
myproduct []     = 1
myproduct (x:xs) = x * myproduct xs
f = myproduct [1,2,3]
main12 = putStrLn(show(f))

(+++) :: [a] -> [a] -> [a]
[]     +++ ys = ys
(x:xs) +++ ys = x : (xs ++ ys)
g = [1,2,3] +++ [5]
main13 = putStrLn(show(g!!3))

myreverse :: [a] -> [a]
myreverse []     = []
myreverse (x:xs) = myreverse xs ++ [x]
h = myreverse "Hello"
main14 = putStrLn(h)

deductFromAccount balance []
  = balance
deductFromAccount balance (d : ds)
  | balance < d = error ("Your account balance is " ++ show balance ++
                         " - cannot deduct " ++ show d ++ " cents")
  | otherwise   = deductFromAccount (balance - d) ds
i = deductFromAccount 75645 [322, 434, 5343, 234]
main15 = putStrLn(show i)

stringToInt :: String -> Int
stringToInt str = stringToIntAcc 0 str
	where
    stringToIntAcc :: Int -> String -> Int
    stringToIntAcc acc []
       = acc
    stringToIntAcc acc (chr : restString) 
       = stringToIntAcc (10 * acc + digitToInt chr) restString
j = stringToInt "12345"
main16 = putStrLn(show j)


fastReverse :: [a] -> [a]
fastReverse xs = reverseAcc [] xs
  where
    reverseAcc :: [a] -> [a] -> [a]
    reverseAcc accList []     = accList
    reverseAcc accList (x:xs) = reverseAcc (x : accList) xs
k = fastReverse [1,2,3,4,5]
main17 = putStrLn(show(k!!0))


sumEvenELems :: Integral a => [a] -> a
sumEvenELems xs = sum (filterEven xs)
	where 
		filterEven :: Integral b => [b] -> [b]
		filterEven [] = []
		filterEven (x:xs)
			| even x = x : filterEven xs
			| otherwise = filterEven xs
l = sumEvenELems [1,2,3,4,5]
main18 = putStrLn(show(l))

sumOfSquareRoots xs = sum (allSquareRoots (filterPositives xs))
  where
    allSquareRoots []     = []
    allSquareRoots (x:xs) = sqrt x : allSquareRoots xs

    filterPositives [] 
      = []
    filterPositives (x:xs)
      | x > 0     = x : filterPositives xs
      | otherwise = filterPositives xs 
m = sumOfSquareRoots [1,2,3,4,5]
main19 = putStrLn(show(m))   

allSquaresMap :: Num a => [a] -> [a]
allSquaresMap xs = map square xs
  where
    square x = x * x
main20 = putStrLn(show(allSquaresMap([1,2,3,4,5])))  

addition a b = a + b
addAll :: Num a => a -> [a] -> [a]
addAll x xs = map (addition x) xs
n = addAll 2 [1,2,3,4,5]
main21 = putStrLn(show(n)) 

average :: Float -> Float -> Float
average a b = (a + b) / 2.0
main22 = putStrLn(show(zipWith average [1, 2, 3] [4, 5, 6])) 

extractDigitsMap :: String -> String
extractDigitsMap strings = filter isDigit strings
main24 = putStrLn(show(extractDigitsMap "4toto")) 

allSquaresMapPointFree = map (\x -> x * x)
main25 = putStrLn(show(allSquaresMapPointFree([1,2,3,4,5]))) 

allEvenFoldr :: [Int] -> Bool
allEvenFoldr = foldr (\x b -> even x && b) True
main26 = putStrLn(show(allEvenFoldr([1,2,3])))

stringToIntFoldl :: String -> Int
stringToIntFoldl = foldl (\acc chr -> 10 * acc + digitToInt chr) 0
main27 = putStrLn(show(stringToIntFoldl("123")))

sumOfSquareRootsCombine :: (Ord a, Floating a) => [a] -> a
sumOfSquareRootsCombine xs = sum (map sqrt (filter (> 0) xs))
main28 = putStrLn(show(sumOfSquareRootsCombine([1,2,3])))

sumOfSquareRootsInfix xs = sum $ map sqrt $ filter (> 0) xs
main29 = putStrLn(show(sumOfSquareRootsInfix([1,2,3])))

sumOfSquareRootsComposition = sum . map sqrt . filter (> 0) 
main30 = putStrLn(show(sumOfSquareRootsComposition([1,2,3])))

type Point = (Float, Float)
type Line  = (Point, Point)
type LineStyle = String
type FancyLine = (Point, Point, LineStyle)
changeLineStyle :: FancyLine -> LineStyle -> FancyLine
changeLineStyle (x, y, _) newStyle
  | newStyle `elem` ["solid", "dashed", "dotted"] = (x, y, newStyle)
  | otherwise 
  = error $ "error in changeLineStyle: " ++ newStyle ++ " is not a valid style"
main31 = putStrLn(show(changeLineStyle ((0,0),(0,0),"toto") "solid"))

data LineStyle2
  = Solid
  | Dashed
  | Dotted
type FancyLine2 = (Point, Point, LineStyle2)
myLine :: FancyLine2
myLine = ((0, 0), (1, 1), Dashed)
changeLineStyle2 :: FancyLine2 -> LineStyle2 -> FancyLine2
changeLineStyle2 (x, y, _) newStyle = (x, y, newStyle)
instance Show LineStyle2 where
   show Solid = "SOLID"
main32 = putStrLn(show(changeLineStyle2 ((0,0),(0,0),Dashed) Solid))

data Day
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Enum)
instance Show Day where
   show Sunday = "Sunday"
   show Monday = "Monday"
   show Tuesday = "Tuesday"
   show Wednesday = "Wednesday"
   show Thursday = "Thursday"
   show Friday = "Friday"
   show Saturday = "Saturday"
main33 = putStrLn(show [Monday .. Friday] )

isWeekday :: Day -> Bool
isWeekday Sunday   = False
isWeekday Saturday = False
isWeekday _        = True

isWeekday2 :: Day -> Bool
isWeekday2 day = case day of
                  Sunday   -> False
                  Saturday -> False
                  _        -> True

isWeekday3 day = not $ day `elem` [Saturday, Sunday]
main34 = putStrLn(show(isWeekday3 Saturday))

data Point2 = Point2 Float Float
           deriving (Show, Eq)

zeroPoint :: Point2
zeroPoint = Point2 0 0

(!+!) :: [a] -> Int -> Maybe a
[]      !+! _ = Nothing
(x : _) !+! 0 = Just x
(_ :xs) !+! n = (!+!) xs (n - 1)

showFifthElement :: Show a => [a] -> String
showFifthElement xs
  = case xs !+! 4 of
      Nothing ->  "there is no fifth element in this list"
      Just n  ->  "the fifth element of the list is: " ++ show n
main35 = putStrLn(show(showFifthElement [1,2,3,4,5]))



isElementSorted :: Ord a  => a -> [a] -> Bool
isElementSorted _ []     = False
isElementSorted a (x:xs)
  | a == x               = True
  | a < x                = False
  | otherwise            = isElementSorted a xs

insertSorted :: (Eq a, Ord a)  => a -> [a] -> [a]
insertSorted x []     = [x]
insertSorted x (y:ys)
  | x <= y            = x : y : ys
  | otherwise         = y : insertSorted x ys

isSorted :: Bool
isSorted = isElementSorted 5 (insertSorted 5 [1,2,3,4,6])
main36 = putStrLn(show (isElementSorted 5 (insertSorted 5 [1,2,3,4,6])))

data BinaryTree a
	= Node a (BinaryTree a) (BinaryTree a)
	| Leaf
	deriving(Show)
main37 = putStrLn(show (Node 5 Leaf (Node 6 Leaf Leaf)))

insertTree :: Ord a => a -> BinaryTree a -> BinaryTree a
insertTree x Leaf
  = Node x Leaf Leaf
insertTree newValue (Node nodeValue leftSubtree rightSubtree)
  | newValue < nodeValue = Node nodeValue (insertTree newValue leftSubtree) rightSubtree
  | otherwise            = Node nodeValue leftSubtree (insertTree newValue rightSubtree)

isElementTree :: Ord a => a -> BinaryTree a -> Bool
isElementTree x Leaf = False
isElementTree value (Node nodeValue leftSubtree rightSubtree)
  | value == nodeValue  = True     
  | value  <  nodeValue = isElementTree value leftSubtree 
  | otherwise           = isElementTree value rightSubtree

tree = Node 5 Leaf (Node 6 Leaf Leaf)
main38 = putStrLn(show (insertTree 4 tree))
main39 = putStrLn(show (isElementTree 5 (insertTree 4 tree)))

data Token
  = PlusTok
  | TimesTok
  | OpenP
  | CloseP
  | IntTok Int
  deriving (Show)

lexer :: String -> [Token]
lexer []              = []
lexer ('+' : restStr) = PlusTok  : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr 
lexer ('(' : restStr) = OpenP    : lexer restStr 
lexer (')' : restStr) = CloseP   : lexer restStr
lexer (chr : restStr) | isSpace chr = lexer restStr
lexer str@(chr : _) 
	| isDigit chr = IntTok (stringToInt digitStr) : lexer restStr
  	where
    	(digitStr, restStr) = break (not . isDigit) str
lexer (chr : restString) 
  = error ("lexer: unexpected character: '" ++ show chr ++ "'")

main40 = putStrLn(show (lexer "2 + 7 * 13"))

data Expr
  = IntLit Int          -- integer constants, leaves of the expression tree
  | Add    Expr Expr    -- addition node
  | Mult   Expr Expr    -- multiplication node
  deriving (Show)

parseInt :: [Token] -> Maybe (Expr, [Token])
parseInt (IntTok n : restTokens)
  = Just (IntLit n, restTokens)
parseInt tokens
  = Nothing

parseProdOrInt :: [Token] -> Maybe (Expr, [Token])
parseProdOrInt tokens
  = case parseInt tokens of
      Just (expr1, (TimesTok : restTokens1)) -> 
          case parseProdOrInt restTokens1 of
            Just (expr2, restTokens2) -> Just (Mult expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result     -- could be 'Nothing' or a valid expression

main41 = putStrLn(show (parseProdOrInt(lexer "2 * 13 * 3 * 11")))

parseSumOrProdOrInt :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrInt tokens
  = case parseProdOrInt tokens of
      Just (expr1, (PlusTok : restTokens1)) -> 
          case parseSumOrProdOrInt restTokens1 of
            Just (expr2, restTokens2) -> Just (Add expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result    -- could be 'Nothing' or a valid expression

main42 = putStrLn(show (parseSumOrProdOrInt(lexer "2 * 13 + 3 + 5")))

parseIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseIntOrParenExpr (IntTok n : restTokens)
  = Just (IntLit n,   restTokens)
parseIntOrParenExpr (OpenP : restTokens1)
  = case parseSumOrProdOrIntOrParenExpr restTokens1 of
       Just (expr, (CloseP : restTokens2)) -> Just (expr, restTokens2)
       Just _  -> Nothing -- no closing paren
       Nothing -> Nothing
parseIntOrParenExpr tokens
  = Nothing
      
parseProdOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseProdOrIntOrParenExpr tokens
  = case parseIntOrParenExpr tokens of
      Just (expr1, (TimesTok : restTokens1)) -> 
          case parseProdOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Mult expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result   
              
parseSumOrProdOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrIntOrParenExpr tokens
  = case parseProdOrIntOrParenExpr tokens of
      Just (expr1, (PlusTok : restTokens1)) -> 
          case parseSumOrProdOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Add expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result

parse :: [Token] -> Expr
parse tokens =
  case parseSumOrProdOrIntOrParenExpr tokens of
    Just (expr, []) -> expr    
    _               -> error "Could not parse input"

main43 = putStrLn(show (parse(lexer "3 + (2 * 13) + (3 + 5)")))

eval :: Expr -> Int
eval (IntLit n) = n
eval (Add expr1 expr2)
  = eval expr1 + eval expr2
eval (Mult expr1 expr2)
  = eval expr1 * eval expr2  

main = putStrLn(show (eval(parse(lexer "3 + (2 * 13) + (3 + 5)"))))
