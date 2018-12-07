module HW7 where

import SyntaxHW7
import ParserHW7

-- Homework6, CS4450/7450
-- Out: November 27th.
-- Due: December 7th by 3pm Central time.

-- You MUST adhere to the following guidelines to get full credit:

-- * Submit (via Canvas): a single file, named HW7_pawprint.hs, where pawprint is
--   your MU username. The file should contain definitions for every function listed below.

-- * Your submission must successfully load and typecheck in Haskell Platform to
--   get any points. For example, executing:
--      $ ghci HW7_pawprint.hs
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

-- There are six questions, each worth 25 points, making this homework
-- worth 150 points total.

--
-- SPECIAL NOTE: Professor Harrison will do almost all of Problem 1 in class on Wednesday,
--               November 28th. So, this problem is almost for free.
--

-- Problem 1. Write a Haskell function that calculates all of the variables that occur free in its
--            argument.
freeProg :: Prog -> [String]
freeProg (fds,stmts) = freeStmts (concat(map freeFD fds)) stmts

freeExp :: [Name] -> Exp -> [Name]
freeExp seen e = case e of
    Add e1 e2    -> freeExp seen e1 ++ freeExp seen e2
    Sub e1 e2    -> freeExp seen e1 ++ freeExp seen e2
    Mul e1 e2    -> freeExp seen e1 ++ freeExp seen e2
    Neg e        -> freeExp seen e
    LitInt _     -> []
    Var x        -> if lkup x seen then [] else [x]
    FunCall f es -> f_free ++ es_free
        where
            f_free  = if lkup f seen then [] else [f]
            es_free = concat (map (freeExp seen) es)

lkup _ []     = False
lkup x (y:ys) = x==y || lkup x ys

freeBool :: [Name] -> BExp -> [Name]
freeBool seen b = case b of
    IsEq e1 e2  -> freeExp seen e1 ++ freeExp seen e2
    IsNEq e1 e2 -> freeExp seen e1 ++ freeExp seen e2
    IsGT e1 e2  -> freeExp seen e1 ++ freeExp seen e2
    IsLT e1 e2  -> freeExp seen e1 ++ freeExp seen e2
    IsGTE e1 e2 -> freeExp seen e1 ++ freeExp seen e2
    IsLTE e1 e2 -> freeExp seen e1 ++ freeExp seen e2
    And b1 b2   -> freeBool seen b1 ++ freeBool seen b2
    Or b1 b2    -> freeBool seen b1 ++ freeBool seen b2
    Not b1      -> freeBool seen b1
    LitBool _   -> [] 

--(Name, [Name], [Stmt])
freeFD :: FunDefn -> [Name]
freeFD (n,ps,body) = freeStmts ps body

freeStmt :: [Name] -> Stmt -> [Name]
freeStmt seen s = case s of
    Assign x e | x `elem` seen -> freeExp seen e
               | otherwise     -> x : freeExp seen e
    If tst sts1 sts2           -> freeBool seen tst ++ freeStmts seen sts1 ++ freeStmts seen sts2
    While tst sts              -> freeBool seen tst ++ freeStmts seen sts
    Let x e sts                -> freeExp seen e ++ freeStmts (x:seen) sts
    Switch e brs               -> freeExp seen e 
                                    ++ freeStmts seen allsts
                                where 
                                    allsts = concat (map snd brs)
    For i e1 e2 sts            -> freeExp seen e1 
                                    ++ freeExp seen e2 
                                    ++ freeStmts (i:seen) sts
    Return e                   -> freeExp seen e


freeStmts :: [Name] -> [Stmt] -> [Name]
freeStmts seen []      = []
freeStmts seen (s:sts) = freeStmt seen s ++ freeStmts seen sts



-- Problem 2. Write a Haskell function that determines if all the defined function names
--            in a given program are unique. E.g., you should not be able to define the
--            same function twice.
uniqueFuns :: Prog -> Bool
uniqueFuns (fds,stmts) = uniqueFns fds []
    where
        uniqueFns [] _         = False 
        uniqueFns (x:xs) visit = if lkupNm x visit then False else True

lkupNm :: FunDefn -> [FunDefn] -> Bool
lkupNm x []                              = False
lkupNm (n,ns,stmts) ((yn,yns,ystmts):ys) = n==yn || lkupNm (n,ns,stmts) ys

-- Problem 3. Write a Haskell function that checks that each function in the program ends with
--            a "Return". That is, literally the last statement in the body of a function must
--            be a Return.
--            ([FunDefn],[Stmt])
returning :: Prog -> Bool
returning (fds,stmts) = and (map returns fds)

returns :: FunDefn -> Bool
returns (n,ns,stmts) = case (last stmts) of
    Assign n e           -> False
    If b stmts1 stmts2   -> False
    While b stmts        -> False
    Let n e stmts        -> False
    Switch e [(i,stmts)] -> False
    For n e1 e2 stmts    -> False
    Return e             -> True


-- Problem 4. Write a Haskell function that checks that formal parameters of a function definition
--            are unique. E.g., "function f(i,j) ..." has unique names but "function f(i,i) ..."
--            does not.
uniqueparams :: Prog -> Bool
uniqueparams (fds,stmts) = and (map params fds)

params :: FunDefn -> Bool
params (n,ns,stmts) = execute ns []
    where 
        execute [] _           = False
        execute (x:xs) visited = if lkup x visited then False else True


-- Problem 5. Write a Haskell function that performs "constant folding". Constant folding means
--            any Exp that does _not_ include a Var or a FunCall is replaced by its value.
--            So, for example, this program:
--    function foo(n) {
--      return (n+2)+3;
--    }
--    y := foo(5+7);
--
-- Constant folding this program would return:
--    function foo(n) {
--      return (n+2)+3;
--    }
--    y := foo(12);
--
-- Note that "(n+2)+3" is not replaced by "n+5" with constant folding. Had the program
-- had "return n+(2+3);" instead, it would have been replaced by "n+5".
cfold :: Prog -> Prog
cfold (fds,stmts) = (fds,(map cfoldStmt stmts))

cfoldStmt :: Stmt -> Stmt
cfoldStmt stmt = case stmt of
    Assign n e           -> Assign n (cfoldExp e)
    If b stmts1 stmts2   -> If b (map cfoldStmt stmts1) (map cfoldStmt stmts2)
    While b stmts        -> While b (map cfoldStmt stmts)   
    Let n e stmts        -> Let n (cfoldExp e) (map cfoldStmt stmts)
    Switch e [(i,stmts)] -> Switch (cfoldExp e) [(i,(map cfoldStmt stmts))]
    For n e1 e2 stmts    -> For n (cfoldExp e1) (cfoldExp e2) (map cfoldStmt stmts)
    Return e             -> Return (cfoldExp e)

cfoldExp :: Exp -> Exp
cfoldExp e = case e of
    Add (LitInt i1) (LitInt i2) -> LitInt (i1+i2)
    Add e1 e2                   -> Add e1 e2 
    Sub (LitInt i1) (LitInt i2) -> LitInt (i1-i2)
    Sub e1 e2                   -> Sub e1 e2
    Mul (LitInt i1) (LitInt i2) -> LitInt (i1*i2)
    Mul e1 e2                   -> Mul e1 e2
    Neg e                       -> Neg (cfoldExp e)
    LitInt i                    -> LitInt i
    Var x                       -> Var x
    FunCall f es                -> FunCall f (map cfoldExp es)


-- cfoldBExp :: BExp -> BExp
-- cfoldBExp b = case b of
--     IsEq e1 e2  -> IsEq (cfoldExp e1) (cfoldExp e2)
--     IsNEq e1 e2 -> IsNEq (cfoldExp e1) (cfoldExp e2)
--     IsGT e1 e2  -> IsGT (cfoldExp e1) (cfoldExp e2)
--     IsLT e1 e2  -> IsLT (cfoldExp e1) (cfoldExp e2)
--     IsGTE e1 e2 -> IsGTE (cfoldExp e1) (cfoldExp e2)
--     IsLTE e1 e2 -> IsLTE (cfoldExp e1) (cfoldExp e2)
--     And b1 b2   -> And (cfoldBExp b1) (cfoldBExp b2)
--     Or b1 b2    -> Or (cfoldBExp b1) (cfoldBExp b2)
--     Not b       -> Not LitBool b || Not (cfoldBExp b)
--     LitBool b   -> LitBool b

 
-- Hint: try writing a helper function, cfoldExp :: Exp -> Exp, that performs constant folding
-- on its input. Make sure that this function works correctly for "(n+2)+3" and "n+(2+3)".

-- Problem 6. Write a static checker that ensures:
--            1. The program has no free variables;
--            2. Has unique function names and each function has unique parameters;
--            3. Ensures each function ends with a Return; and
--            4. If the program passes 1.-3., perform constant folding and return the result.

staticcheck :: Prog -> Maybe Prog
staticcheck (fds,stmts) = if ((length (freeProg (fds,stmts)) /= 0) && uniqueFuns (fds,stmts) && uniqueparams (fds,stmts) && returning (fds,stmts))
                            then Just (cfold(fds,stmts))
                            else Nothing

--basically, I need a way to run this
examples :: [String]
examples = ["examples/exam" ++ (show x) ++ ".imp" | x <- [0..9]]

asts :: IO [Prog]
asts = sequence $ map parseImp examples

checkp :: (Prog -> a) -> IO [a]
checkp f = do a <- asts; 
              return $ map f a