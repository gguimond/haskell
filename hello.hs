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

