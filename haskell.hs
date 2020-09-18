
import Data.Char

add :: Integer -> Integer -> Integer
add a b = a + b
addpartial = add 5
fib x
  | x  < 2 = 1
  | otherwise = fib (x -1 ) + fib (x-2)
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)
compo = addpartial . fib

li :: [String]
li = ["help"]
for array func = map func array

data Color = Red | Blue | Green | Yellow

data Point = Point Float Float
distance :: Point -> Point -> Float
distance (Point x y) (Point x' y') = sqrt $ dx + dy
    where dx = (x - x') ** 2
          dy = (y - y') ** 2
p1 = Point 1 1
p2 = Point 2 2

data Maybe a = Nothing | Just a

instance Eq Color where  
  Red == Red = True  
  Green == Green = True  
  Yellow == Yellow = True  
  _ == _ = False 

data Point2 = Point2 Float Float deriving (Eq, Read, Show)

countLines :: String -> String
countLines = show . length . lines

sayHello :: IO ()
sayHello = do
   putStrLn "What is your name?"
   name <- getLine -- this gets a line and gives it the name "name"
   putStrLn $ "Hello, " ++ name


qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
  where lesser  = filter (< p) xs
        greater = filter (>= p) xs

natSum :: (Num a, Ord a) => a -> a
natSum 0              = 0
natSum n  
  | n > 0     = n + natSum (n - 1) 
  | otherwise = error "natSum: Input value too small!"

extractDigits :: String -> String
extractDigits []
  = []
extractDigits (chr : restString)
  | isDigit chr = chr : extractDigits restString
  | otherwise   =       extractDigits restString

product2 :: Num a => [a] -> a
product2 []     = 1
product2 (x:xs) = x * product xs

reverse2 :: [a] -> [a]
reverse2 []     = []
reverse2 (x:xs) = reverse2 xs ++ [x]

fastReverse :: [a] -> [a]
fastReverse xs = reverseAcc [] xs
  where
    reverseAcc :: [a] -> [a] -> [a]
    reverseAcc accList []     = accList
    reverseAcc accList (x:xs) = reverseAcc (x : accList) xs

sumEvenElems :: Integral a => [a] -> a
sumEvenElems []
  = 0
sumEvenElems (x:xs)
  | even x    = x + sumEvenElems xs
  | otherwise = sumEvenElems xs

sumOfSquareRoots xs = sum (allSquareRoots (filterPositives xs))
  where
    allSquareRoots []     = []
    allSquareRoots (x:xs) = sqrt x : allSquareRoots xs

    filterPositives [] 
      = []
    filterPositives (x:xs)
      | x > 0     = x : filterPositives xs
      | otherwise = filterPositives xs                

getEvens :: [Integer] -> [Integer]
getEvens ints = filter even ints


getEvensPointFree :: [Integer] -> [Integer]
getEvensPointFree = filter even

mysum (x:xs) = x + mysum xs
mysum [] = 0

mysumFold xs = foldr (+) 0 xs

mysumFoldPointFree = foldr (+) 0

myreverse :: [a] -> [a]
myreverse = foldr (\x xs -> xs ++ [x]) []
-- foldr op n (x:xs) = x `op` foldr n op xs

myreverse2 :: [a] -> [a]
myreverse2 = foldl (\accList x -> x : accList) []
-- foldl op acc (x:xs) = foldl op (acc `op` x) xs

mytotalsum :: Integral a => [a] -> a
mytotalsum = foldl (\acc x -> x + acc) 0

mytotalsum2 :: Integral a => [a] -> a
mytotalsum2 = foldr (\acc x -> x + acc) 0

sumOfSquareRoots2 :: (Ord a, Floating a) => [a] -> a
sumOfSquareRoots2 xs = sum (map sqrt (filter (> 0) xs))

sumOfSquareRoots3 xs = sum $ map sqrt $ filter (> 0) xs
sumOfSquareRoots4 = sum . map sqrt . filter (> 0)

type Point10 = (Float, Float)

data Day
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Enum)

isWeekday :: Day -> Bool
isWeekday day = not $ day `elem` [Saturday, Sunday]

data Colour
  = Colour { redC      :: Int
           , greenC    :: Int
           , blueC     :: Int
           , opacityC  :: Int
           }
  deriving (Show, Eq)

red :: Colour
red = Colour{redC = 255, opacityC = 255, blueC = 0, greenC = 0}
greenComponent :: Colour -> Int
greenComponent Colour{greenC = green} = green

zeroX :: Float -> Point
zeroX = Point 0

data BinaryTree a
  = Node a (BinaryTree a) (BinaryTree a)
  | Leaf

bt :: BinaryTree Integer
bt = Node 5 (Node 6 Leaf Leaf) Leaf

insertTree :: Ord a => a -> BinaryTree a -> BinaryTree a
insertTree x Leaf
  = Node x Leaf Leaf
insertTree newValue (Node nodeValue leftSubtree rightSubtree)
  | newValue < nodeValue = Node nodeValue (insertTree newValue leftSubtree) rightSubtree
  | otherwise            = Node nodeValue leftSubtree (insertTree newValue rightSubtree)

isElementTree :: Ord a => a -> BinaryTree a -> Bool
isElementTree x Leaf
  = False
isElementTree value (Node nodeValue leftSubtree rightSubtree)
  | value == nodeValue = True
  | value < nodeValue = isElementTree value leftSubtree
  | otherwise = isElementTree value rightSubtree

bt2 = insertTree 7 bt

main = do
let x = add 1 2
print x
print (addpartial 2)
let y = fib x
print y
print (myMap (\x -> x + 2) [1..5])
print (compo 5)
print (fib 5)
print (fib 7)
print (even (fib 7))
print (even $ fib 7)

if 1 == 1
  then print "awesome"
  else print "awful"

case li of
  ("help":x) -> print "help"
  ("start":x) -> print "start"
  _ -> print "wrong"

print(for [0..5] $ \i -> i)
print(for [0..5] addpartial)

putStrLn $ "Hello, sky! " ++ "eah"

print (countLines "toto\ntiti")

-- sayHello
print (natSum 3)

print (qsort [3,2,1])

print (extractDigits ['3','2','t'])

print (product2 [1,2,3])

print (reverse2 ['h','e','l','l','o'])

print (fastReverse ['h','e','l','l','o'])

print (sumEvenElems [1,2,3,4])

print (sumOfSquareRoots [1,2,3,4])

print (map (\x -> x + 2) [1,2,3,4])

print (map addpartial [1,2,3,4])

print (zipWith add [1,2,3,4] [1,2,3,4])

print (getEvens [1,2,3])

print (getEvensPointFree [1,2,3])

print (mysumFoldPointFree [1,2,3])

print (myreverse [1,2,3])

print (mytotalsum [1,2,3])

print (mytotalsum [1,2,3])

print (sumOfSquareRoots2 [1,2,3])

print (sumOfSquareRoots3 [1,2,3])

print (sumOfSquareRoots4 [1,2,3])

print (isWeekday Monday)

print (isElementTree 7 bt2)