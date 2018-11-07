module HW5 where

import Parsing

-- Due: Wednesday, November 7, by 11:59pm.

-- You MUST adhere to the following guidelines to get full credit:

-- * Submit (via Canvas): a single file, named HW5_pawprint.hs, where pawprint is
--   your MU username. The file should contain definitions for every function listed below.

-- * Your submission must successfully load and typecheck in Haskell Platform to
--   get any points. For example, executing:
--      $ ghci HW5_pawprint.hs
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

-- There are two questions, each worth 25 points, making this homework
-- worth 50 points total.

--This is the code "SimpleTypes.hs" with some annotations.

-- Problem 1. Add abstract syntax for tuple types. A tuple type will take the form
-- familiar from Haskell. That is, they should have the form (t1,...,tn) where n > 1.
-- This boils down to adding an appropriate constructor to the Ty data type.

-- Problem 2. Extend the parser for simple types with a parser called "parseTuple" which
-- parses tuple types. Or, in other words, fill in the definition below. Hint: read the Parsing
-- slides carefully. To complete this problem, you will also have to extend the definition of
-- parseTy to use parseTuple.

parseTuple :: Parser Ty 
parseTuple = do
  symbol "("
  t1 <- parseTy
  t2 <- many1 (do symbol ","
                  parseTy)
  symbol ")"
  return (Tuple (t1:t2))
 

-- These should succeed.
ex1 = parse parseTy "(Number,Number,Boolean)"
ex2 = parse parseTy "((Number,Boolean) -> Number)"
ex3 = parse parseTy "(((Number,Boolean) -> Number) -> (Number -> (Boolean -> Number)))"

-- These should fail (i.e., return []).
fail1 = parse parseTy "()"
fail2 = parse parseTy "(Number)"

data Ty = Number | Boolean | Tuple [Ty] | Arrow Ty Ty deriving Show

parseNumber, parseBoolean, parseArrow, parseTy :: Parser Ty

parseNumber  = do
  symbol "Number"
  return Number

parseBoolean = do
  symbol "Boolean"
  return Boolean

parseArrow = do
  symbol "("
  t1 <- parseTy
  symbol "->"
  t2 <- parseTy
  symbol ")"
  return (Arrow t1 t2)

parseTy = parseBoolean +++ parseNumber +++ parseArrow +++ parseTuple
