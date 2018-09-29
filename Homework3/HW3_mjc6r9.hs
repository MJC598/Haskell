module HW3 where

import Prelude hiding (foldr,map,(++))
import Data.List hiding (map,(++),foldr)

-- Due: Tuesday, October 2, by 3pm.

-- You MUST adhere to the following guidelines to get full credit:

-- * Submit (via Canvas): a single file, named HW3_pawprint.hs, where pawprint is
--   your MU username. The file should contain definitions for every function listed below.

-- * Your submission must successfully load and typecheck in Haskell Platform to
--   get any points. For example, executing:
--      $ ghci HW3_pawprint.hs
--   should not produce any errors. We won't attempt to grade assignments that 
--   fail to load, meaning you'll get zero points for this homework. Zero. Points.

-- * Name all functions and data types exactly as they appear in the
--   assignment. Grading will be partly automated, so incorrectly named functions are
--   likely to be counted as undefined functions.

-- * The code you submit must be your own. Exceptions: you may (of course) use
--   the code we provide however you like, including examples from the slides and the
--   books.

-- * No late submissions---PLEASE START EARLY!

-- For each of the following questions, put your answer directly below the 
-- question. 

-- (1) You must use a text editor (e.g., vi, textpad, emacs, etc.) to prepare 
--     your solution. 
-- (2) You must write type declarations for each and every one of your Haskell
--     definitions.
-- (3) The program you turn in must be the product of your effort alone.

-- There are eight questions, each worth 10 points, making this homework
-- worth 80 points total. Some are harder than others.

-- Here are the definitions of foldr and map:

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (a:as) = a `f` (foldr f v as)

map :: (a -> b) -> [a] -> [b]
map f = foldr (\ a bs -> f a : bs) []

-- Problem 1. Write a function, range :: Int -> Int -> [Int], so that (range i j) is
-- the list of Int's between i and j, in order, including i and j. The simplest way
-- to write range is just: range i j = [i..j]. You may not use "Texas ranges" (as LYAHGG
-- calls them) in your solution however. Instead, use guards to distinguish the cases
-- where i==j, i<j, and i>j. In this last case, use the error function to signal
-- an error.
--
-- Your answer should behave as follows:
--
-- ghci> range 1 10
--    [1,2,3,4,5,6,7,8,9,10]
-- ghci> range 1 1
--    [1]
-- ghci> range 10 1
--    *** Exception: Invalid Input

range :: Int -> Int -> [Int]
range i i = [i]
range i j = if i > j
            then error "empty"
            else i : range (i+1) j 

-- Problem 2. Define a recursive function euclid :: Int -> Int -> Int that implements
-- Euclid’s algorithm for calculating the greatest common divisor of two non-negative 
-- integers: if the two numbers are equal, this number is the result; otherwise the 
-- smaller number is subtracted from the larger, and the same process is then repeated. 
-- For example, euclid 6 27 should return the result 3.

euclid :: Int -> Int -> Int
euclid = undefined

-- Problem 3. Add type declarations for the following functions. Obviously, one thing
-- you can do is just load this file into GHCi and use ":t" to determine their types.
-- So, there's no reason to lose any points on this problem. BUT, try to figure out their
-- types by looking at the code first. Can you make sense of why they have the types 
-- they do?

h1 = (\ (x,y) -> x + y)

h2 = (\ (x,y) -> (y,x)) 

f1 = map (\ (x,y) -> x + y) . map (\ (x,y) -> (y,x)) 

f2 = map fst . map snd

f3 = map snd . map fst

-- Problem 4. Add a type declaration for the following function. This is just like the
-- previous question, but it's a little more challenging. Try to figure out its type
-- without using ":t" and then check your answer with ":t".

fuse f g = (map f . map g, map (f . g))

-- Problem 5. There's a program optimization used in the GHC compiler called "map fusion"
-- which is based on the following equality:
--      map f . map g == map (f . g)
-- for all appropriately typed functions. This equation above is a mathematical theorem
-- (and not some sort of Haskell definition or expression). Rewrite the definitions of 
-- f1, f2, and f3 below with their "fused" versions. Remember to add type declarations
-- and, of course, the types of fusedf1 and f1, etc., should be identical.

fusedf1 = undefined

fusedf2 = undefined

fusedf3 = undefined

-- Problem 6. The switcheroo function takes a single argument, which
-- is itself a function of two arguments, and returns a function with the arguments
-- reversed. For example, consider the cons constructor for lists; here's its type:
--
-- ghci> :t (:)
--    (:) :: a -> [a] -> [a]
--
-- When we apply switcheroo to cons, the resulting function takes the tail of the list
-- first and the head of the list second:
--
-- ghci> :t switcheroo (:)
--    switcheroo (:) :: [a] -> a -> [a]
--
-- Write switcheroo by replacing the call to error with a lambda expression. To receive
-- credit, your answer *must* be a lambda expression.

switcheroo :: (a -> b -> c) -> (b -> a -> c)
switcheroo f = error "replace this with a lambda expression"

--
-- Problem 7. Here's (almost) a definition of append:
--

app :: [a] -> [a] -> [a]
app ys []     = ys
app ys (x:xs) = x : app ys xs 

--
-- Redefine app using foldr. Look at the examples from the slides (e.g., iadd, sapp,
-- etc.). The answer is a "one liner".
--

appAsFoldr :: [a] -> [a] -> [a]
appAsFoldr = foldr (:)

-- Note that, app and appAsFoldr are not quite (++):
-- ghci> appAsFoldr [1,2,3] [4,5]
--    [4,5,1,2,3]

--
-- Problem 8. Combine your answers to Problems 6 and 7 to define append.
--

(++) :: [a] -> [a] -> [a]
(++) = switcheroo appAsFoldr
