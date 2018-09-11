module HW1_mjc6r9 where

import Prelude hiding (filter)

type Complex = (Float,Float)

-- Problem 1. Define a function, magn, that computes the magnitude of a complex
-- number. Recall that the magnitude of a complex number, a+bi, is defined
-- as: squareroot (a^2 + b^2). You can use the built-in Haskell function, sqrt,
-- in your answer. Put your definition below:

magn :: Complex -> Float
magn c = sqrt((fst c)^2 + (snd c)^2)

-- Problem 2. Define a function, add, that takes two complex numbers and adds them.
-- Recall that (a + bi) + (c + di) = (a+c) + (b+d)i.

add :: Complex -> Complex -> Complex
add com1 com2 = ((fst com1 + fst com2),(snd com1 + snd com2))


-- Problem 3. Define a function, sub, that subtracts two complex numbers.
-- Recall that (a + bi) - (c + di) = (a-c) + (b-d)i.
sub :: Complex -> Complex -> Complex
sub com1 com2 = ((fst com1 - fst com2),(snd com1 - snd com2))

-- Problem 4. Define a function, mult, that multiplies two complex numbers.
-- Recall that (a + bi) * (c + di) =  (a*c - b*d) + (a*d + b*c)i
mult :: Complex -> Complex -> Complex
mult com1 com2 = ((fst com1 * fst com2 - snd com1 * snd com2),(fst com1 * snd com2 + snd com1 * fst com2))

-- Problem 5. Here is how a function called filter is defined in the Haskell
-- standard prelude:

filter :: (a -> Bool) -> [a] -> [a]
filter p []     = []
filter p (x:xs) = if p x
                    then x : filter p xs
                    else filter p xs

-- (filter p l) collects together the list elements of l on which p is true.
-- Ex. ghci> filter odd [1,2,3,4,5]
--     [1,3,5]
--
-- Note that the function "odd" is built-in to Haskell.
--
-- Write a function, sqrodd, that takes a list of Ints and returns the same
-- list except that any odd number is squared.
-- Ex. ghci> sqrodd [1,2,3,4,5]
--     [1,2,9,4,25]
-- You must write the function from "scratch" --- i.e., you can't use any
-- other built-in Haskell function except odd. Hint: the code for sqrodd
-- will look eerily similar to filter.



sqrodd :: [Int] -> [Int]
sqrodd []     = []
sqrodd (x:xs) = if odd x
                    then x*x : sqrodd xs
                    else x : sqrodd xs



-----------------------------------------------------------------
-----------------------------------------------------------------
-----------------------------------------------------------------

-- Recall from class the abstract syntax for propositional logic and
-- its expression as a Haskell data type:

type Var = String
data Prop = Atom Var
          | Not Prop
          | Imply Prop Prop
          | And Prop Prop   -- this is new

-- Terminology. We say that (And p q) is the "conjunction" of p and q.

-- Recall, we can represent (- p) as the following Haskell value:

negp :: Prop
negp = Not (Atom "p")

-- Problem 6. Define "(a => (b => c)) => ((a & b) => c)" using the above
-- abstract syntax. Write your answer

prob6 :: Prop
prob6 = Imply (Imply a (Imply b c)) (Imply (And a b) c) 
      where
        a = Atom "a"
        b = Atom "b"
        c = Atom "c"

-- Below are the Show and Eq instances for Prop that we gave in class.

-- Problem 7. Define show for And below, using ampersand (i.e., '&') for And.
-- Check your answer to this question against your answer to Problem 6.
instance Show Prop where
  show (Atom p)            = p
  show (Not prop)          = "(-" ++ show prop ++ ")"
  show (Imply prop1 prop2) = "(" ++ show prop1 ++ " => " ++ show prop2 ++ ")"
  show (And pr1 pr2)       = "(" ++ show pr1 ++ " & " ++ show pr2 ++ ")"

-- Problem 8. Define == for And below.
instance Eq Prop where
  (Atom p) == (Atom q)       = p == q
  (Not x) == (Not y)         = x == y
  (Imply x y) == (Imply u v) = (x == u) && (y == v)
  (And a b) == (And c d)     = (a == c) && (b == d) --Check that the first 2 props equals the next two props 
  _ == _                     = False


--Problem 9.

-- The next question concerns writing a definitional extension of the Prop language
-- as defined above. See "Defined Connectives as Functions" in the SyntaxVsSemantics slides
-- for background explanation. The "Sheffer stroke" (https://en.wikipedia.org/wiki/Sheffer_stroke)
-- for propositional sentences p and q can be defined as the negation of the conjunction of
-- p and q. What you will do is analogous to what we did in class with the "orPL", "andPL", and
-- "iffPL" functions (see the aforementioned slides).

--used to help sheffer function
orPL :: Prop -> Prop -> Prop
orPL p q = Imply (Not p) q

--sheffer function is inverse of And, so basically Not andPL (or remove not already present in andPL)
sheffer :: Prop -> Prop -> Prop
sheffer p q = orPL (Not p) (Not q)

-- Problem 10.

-- The next question concerns writing a function that collects all of the propositional
-- variables in a Prop. You will write a function, vars, that takes a Prop as input and
-- produces a list of String as output. The type template for this function is below.

-- Review "HaskellForGrownups" slides and, in particular, "How to Write a Haskell Function"
-- slides. The smart way to do this problem is in a stepwise fashion. That is, (1) replace
-- a single undefined with what you think the correct definition is and, then, (2) load that
-- into ghci to see if it typechecks. Your answer may produce repeated entries. For example,
-- evaluating (vars prob6) with ghci may return ["a","b","c","a","b","c"].

--This needs a parenthesis around the parameters for var when run in ghci
vars :: Prop -> [String]
vars (Atom p)      = [p]
vars (Not p)       = vars p
vars (Imply p1 p2) = vars p1 ++ vars p2
vars (And p1 p2)   = vars p1 ++ vars p2
