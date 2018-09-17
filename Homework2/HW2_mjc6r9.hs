module HW2 where

-- Due: Wednesday, September 19, by 3pm.

-- 1. Fill in the gaps. Complete the following definitions. It does not
--    matter what the definitions do as long as they are type correct. For e1, e2,
--    e5, and e7, this means uncommenting the type declaration and replacing the ? with
--    type(s) correspond to the definitions directly below them. For e3, e4,
--    e6, and e8, this means replacing the undefined with an expression of appropriate type.
--
--    Note: you should complete each definition one at a time.

e1 :: [Bool]
e1 = [True,False,True]

e2 :: [[Int]]
e2 = [[1,2],[3,4]]

e3 :: (Char,Bool)
e3 = ('a', True)

e4 :: [(String,Int)]
e4 = [("hello", 1), ("world", 2)]

e5 :: Int -> Int
e5 n = n*2

e6 :: Int -> Int -> Int
e6 n k = n*k

e7 :: (a,a) -> (a,a)
e7 (x,y) = (y,x)

e8 :: a -> (a,a)
e8 n = (n,n)

-- 2. Add type declarations to the following definitions. Try to give the most general types possible.
nums :: [Int]
nums = [1,2,3,4,5]

tables :: [(Bool, Int)]
tables = [(False,1),(True,2),(False,3)]

one :: a -> [a]
one x = [x]

three :: a -> (a,a,a)
three x = (x,x,x)

first :: a -> b -> a
first x y = x

mult :: Int -> Int -> Int
mult m n = m * n

-- 3. The function, second :: [a] -> a, returns the second element in a list that contains at least
--    this many elements. Define three different versions of this function using:
-- (a) head and tail
-- (b) list indexing !!
-- (c) pattern matching
--
-- Name your functions second1, second2 and second3 and define them below.
second1 :: [a] -> a
second1 []    = error "empty"
second1 xs    = head (tail xs) 

second2 :: [a] -> a
second2 [] = error "empty"
second2 xs = xs !! 1

second3 :: [a] -> a
second3 []       = error "empty"
second3 (a:b:xs) = b 


-- 4. The exclusive-or function, xor :: Bool -> Bool -> Bool, takes two Bool values and returns
--    True when precisely one is True, returning False otherwise. Define three versions of this
--    function using:
-- (a) pattern matching
-- (b) if then else
-- (c) the operator /= (not equal to)
--
-- Name your functions xor1, xor2 and xor3. Make your definitions as simple as possible.
xor1 :: Bool -> Bool -> Bool
xor1 True False  = True
xor1 False True  = True
xor1 False False = False
xor1 True True   = False 

xor2 :: Bool -> Bool -> Bool
xor2 a b = if a == True 
            then if b == False 
                then True 
                else False 
            else if b == True 
                then True 
                else False


xor3 :: Bool -> Bool -> Bool
xor3 a b = if a /= b 
        then True 
        else False

-- 5. Using a list comprehension, define a function, sumsqr :: Int -> Int, that calculates
--    the sum of the first n integer squares. For example, sumsqr 3 = 1^2 + 2^2 + 3^2 = 14. You
--    may assume that n >= 0.

sumsqr :: Int -> Int
sumsqr n = sum [n^2 | n <- [0..n]] 

--6. Using a list comprehension, define a function grid :: Int -> [(Int,Int)] that returns a
--   list of all (x, y) coordinate pairs on an n X n square grid, excluding the diagonal
--   running from (0, 0) to (n, n). For example,
--
--     ghci> grid 2
--        [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]

grid :: Int -> [(Int,Int)]
grid n = [(a,b) | a <- [0..n], b <- [0..n], a /= b]

-- 7. Define the function snoc :: a -> [a] -> [a], which takes an item and a list of items as
--    input, and returns the list with the item at the end. For example:
--        ghci> snoc 9 [1,2,3,4]
--           [1,2,3,4,9]
--
--    Notice that snoc puts an item at the end of a list and not at the head like cons.
--    To receive credit, you cannot use append (++) – you must use recursion and cons (:) only.

snoc :: a -> [a] -> [a]
snoc a []     = [a]
snoc a (x:xs) = x : snoc a xs 

-- 8. Consider the following definition of list reversal:
--
{-
rev :: [a] -> [a]
rev xs = rev' [] xs
    where rev' :: [a] -> [a] -> [a]
        rev' acc []     = acc
        rev' acc (x:xs) = rev' (x:acc) xs
-}

-- The helper function rev' an example of "accumulator passing style" (APS). With APS, an
-- intermediate value is passed along (a.k.a., "accumulated"). For reasons we will discuss in
-- class, APS can be, in some circumstances, a much more efficient style than the naive way
-- we have written reverse in class.

-- Write a function, maxaps, that, when given a list of non-negative Int's, returns the maximum
-- number in the list.

maxaps :: [Int] -> Int
maxaps xs = maxaps' 0 xs
    where maxaps' :: Int -> [Int] -> Int
          maxaps' n [] = n
          maxaps' n (x:xs) = if x > n 
                             then maxaps' x xs 
                             else maxaps' n xs

-- 9. Write a function, sumaps, which calculates the sum of a list on Int's in accumulator passing
--    style.

sumaps :: [Int] -> Int
sumaps xs = sumaps' 0 xs
    where sumaps' :: Int -> [Int] -> Int
          sumaps' n []     = n
          sumaps' n (x:xs) = n + sumaps' x xs 

-- 10. Recall the usual factorial function definition:
--
fac 0 = 1
fac n = n * fac (n-1)

-- Write a function, facaps, that calculates factorial in accumulator passing style.
facaps :: Int -> Int
facaps n = facaps' n 0
    where facaps' :: Int -> Int -> Int
          facaps' 0 counter = 1
          facaps' n counter = n * facaps' (n-1) (counter+1)   
