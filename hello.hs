-- http://learn.hfm.io/index.html

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
main = putStrLn(show t2) 
