module HW6 where

import WhileAS
import While 
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )


-- Homework6, CS4450/7450
-- Out: November 9th.
-- Due: November 16th by 3pm Central time.

-- You MUST adhere to the following guidelines to get full credit:

-- * Submit (via Canvas): a single file, named HW6_pawprint.hs, where pawprint is
--   your MU username. The file should contain definitions for every function listed below.

-- * Your submission must successfully load and typecheck in Haskell Platform to
--   get any points. For example, executing:
--      $ ghci HW6_pawprint.hs
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

-- There are four questions, each worth 25 points, making this homework
-- worth 100 points total.

{-
Problem 1. Define instances of the Show class for Stat, AExp and BExp
-}

instance Show BExp where
  show (BUnOp op b1)    = show (op) show (b1)
  show (BoolLit True)   = show (True)
  show (BoolLit False)  = show (False)
  show (RelOp op e1 e2) = show (op) show (e1) show (e1)

instance Show Stat where
  show (Assign x e _) = show (x) show (e)
  show (Skip _)       = None
  show (Seq sts)      = show (sts)
  show (While b _ s)  = show (b) show (s)
  show (NewInt x s)   = show (x) show (s)

instance Show AExp where
  show (Var x)        = show (x)
  show (IntLit e)     = show (e)
  show (AOp op e1 e2) = show (op) show (e1) show (e2)

{-
Problem 1 (continued). Test your answer with the function defined below.
-}
front fname
  = do{ input <- readFile fname
      ; putStr input
      ; case parse program fname input of
           Left err -> error (show err)
           Right x  -> return x
      }

{-
Problem 2. Below is the interpreter for the simple imperative language which we discussed in
class. There are several questions inserted in the code below. Answer them by extending 
the interpreter. Be sure to test your answers.
-}

type Memory = [(Ident, Int)]
type Trans = (Stat, Memory) -> Memory

--
-- memory look-up
--
lkup :: Memory -> Ident -> Int
lkup ((x, n):ms) x' = if x == x' then n else lkup ms x'
lkup [ ] x'         = error ("Unbound variable: " ++ x')

--
-- evaluate an arithmetic expression:
--
evE (Var i) m        = lkup m i
evE (IntLit i) m     = i
evE (AOp op e1 e2) m = case op of
                         "+" -> evE e1 m + evE e2 m
                         "*" -> evE e1 m * evE e2 m
                         "-" -> evE e1 m - evE e2 m
                         _   -> error ("Undefined operator: " ++ op)
                            
evB (BUnOp "not" b) m    = not (evB b m)
evB (BoolLit b) m        = b
evB (BOp "&" b1 b2) m    = evB b1 m && evB b2 m
evB (RelOp ">" e1 e2) m  = evE e1 m > evE e2 m
evB (RelOp "<=" e1 e2) m = evE e1 m <= evE e2 m

exec :: Stat -> Memory -> Memory
exec (Skip _) m       = m
exec (Assign i e _) m = (i,evE e m) : m
-- 
-- Problem 2 a.
-- The definition below for Seq handles sequences of statements of precisely length 2.
-- Redefine this clause for the general case in which there are arbitrary numbers of 
-- statements.
--
exec (Seq x:xs) m  = exec x (exec (Seq xs) m)
exec (Seq []) m    = []
exec (If b _ s1 s2) m = if evB b m
                           then
                              exec s1 m
                           else
                              exec s2 m
exec (While b l c) m  = if evB b m 
                           then
                              exec (Seq [c,While b l c]) m
                           else
                              m
-- 
-- Problem 2 b.
-- The (NewInt x c) statement defines a variable x that may be used exclusively within c.
-- The effect of (NewInt x c) on the memory m should return a new memory m' in which
-- x is initialized to 0 (but is otherwise identical to m).
--
-- Define NewInt below.
--
exec (NewInt x c) m   = error "NewInt exec undefined"

{- Problem 3.
A static check commonly used in language implementations determines if all variables used 
in the program are declared by a NewInt first. For an example, look at fib.wh and newfib.wh. 

// fib.wh uses undeclared variables.
    v := 1; 
    u := 1; 
    if n <= 2 then 
      skip 
    else 
      while n > 2 do (
        t := u; 
        u := v; 
        v := u + t
        )

// newfib.wh declares all variables that it uses.
    newint v in (
    newint u in (
           v := 1; 
           u := 1; 
       newint n in (
         newint t in (
           if n <= 2 then 
             skip 
           else 
             while n > 2 do (
               t := u; 
               u := v; 
               v := u + t
         )
   ))
))
    
Below is a code outline for this static check. (checkS s []) is True if s contains no
undeclared variables. (check (NewInt x c) ds) records in ds that x is declared when it 
recursively checks c. Complete the definitions of checkS, checkE and checkB. Test your 
solution with check below. In particular, (check "newfib.wh") should be True and 
(check "fib.wh") should be False.
-}

checkS :: Stat -> [Ident] -> Bool
checkS c r = case c of 
                 (Skip _)       -> undefined
                 (Assign x e _) -> undefined
                 (Seq cs)       -> undefined
                 (If b _ s1 s2) -> undefined
                 (While b _ c)  -> undefined
                 (NewInt i c)   -> undefined

checkE :: AExp -> [Ident] -> Bool
checkE e r = case e of 
                  (Var x)        -> undefined
                  (IntLit i)     -> undefined
                  (AOp op e1 e2) -> undefined
                  
checkB :: BExp -> [Ident] -> Bool
checkB b r = case b of
                  (BUnOp "not" b)    -> undefined
                  (BoolLit b)        -> undefined
                  (BOp "&" b1 b2)    -> undefined
                  (RelOp ">" e1 e2)  -> undefined
                  (RelOp "<=" e1 e2) -> undefined

check fname
  = do{ input <- readFile fname
      ; putStr input
      ; case parse program fname input of
           Left err -> error (show err)
           Right x  -> return (checkS x [])
      }

{-
Problem 4. Write a read-eval-print loop that (a) reads a file name from the prompt, (b) 
calls the parser on it, (c) runs checkS of the program, and, (d) if there are no undeclared 
variables, runs the exec interpreter on it. Finally, the loop should start again. Your 
solution will look something like the check function above. Consult the lecture slides
for more information about read-eval-print loops.
-}
