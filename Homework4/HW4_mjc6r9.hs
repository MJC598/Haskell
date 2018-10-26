module HW4 where

--Due: Friday October 26, by 3pm

-- solveRPN :: String -> Float
-- solveRPN = head . foldl foldingFunction [] . words
--     where   foldingFunction (x:y:ys) "*"    = (x * y):ys
--             foldingFunction (x:y:ys) "+"    = (x + y):ys
--             foldingFunction (x:y:ys) "-"    = (x - y):ys
--             foldingFunction xs numberString = read numberString:xs

data Op   = Val Int | Plus | Minus | Mul | IntDiv deriving (Show, Eq)
type PExp = [Op] 

--parse the string into a list of Ops
rpnParse :: String -> PExp
rpnParse x = tester (words x)

tester :: [String] -> PExp
tester []       = []
tester ("+":ps) = Plus:(tester ps)
tester ("-":ps) = Minus:(tester ps)
tester ("*":ps) = Mul:(tester ps)
tester ("/":ps) = IntDiv:(tester ps)
tester (p:ps)   = (Val (read p)):(tester ps)

-- --evaluate the function after parsing
-- eval :: PExp -> Int
-- eval []  = error "Bad Input"
-- eval (xs) = intConverter(head(foldl foldingFunction [] xs))
-- 
-- foldingFunction :: PExp -> Op -> PExp
-- foldingFunction [] _            = error "Bad Input"
-- foldingFunction (x:y:ys) Plus   = (Val(intConverter(x) + intConverter(y))):ys
-- foldingFunction (x:y:ys) Minus  = (Val(intConverter(x) - intConverter(y))):ys
-- foldingFunction (x:y:ys) Mul    = (Val(intConverter(x) * intConverter(y))):ys
-- foldingFunction (x:y:ys) IntDiv = (Val(intConverter(x) / intConverter(y))):ys
-- foldingFunction xs numberString = numberString:xs

-- intConverter :: Op -> Int
-- intConverter Val(x) = x 

-- --the safe evaulation function
-- evalSafe :: PExp -> RPNResult
-- evalSafe [] = InvalidInput
-- evalSafe ps = head . foldl folding [] ps
--             where folding :: PExp -> Op -> PExp
--                   folding (x:y:ps) Plus           = (x + y):ps
--                   folding (x:y:ps) Minus          = (x - y):ps
--                   folding (x:y:ps) Mul            = (x * y):ps
--                   folding (x:y:ps) IntDiv         = if y == 0
--                                                     then DivByZero
--                                                     else (x / y):ps
--                   -- folding ((x:ps) (Val (read x))) = x:ps
--new data sets and types
data RPNError   = DivByZero | InvalidInput deriving (Show, Eq)
type RPNResult  = Either RPNError Int


--translation into infix
rpnTrans :: PExp -> Either (String) (RPNError)
rpnTrans xs          = infixConvert xs []

infixConvert :: PExp -> [String] -> Either (String) (RPNError)
--condition if it works correctly
infixConvert [] (x:[])                   = Left x
infixConvert ((Val x):remain) (xs)       = infixConvert remain ((show x):xs)
infixConvert (Plus:remain) (x:y:xs)      = infixConvert remain (("(" ++ y ++ "+" ++ x ++ ")"):xs)
infixConvert (Minus:remain) (x:y:xs)     = infixConvert remain (("(" ++ y ++ "-" ++ x ++ ")"):xs)
infixConvert (Mul:remain) (x:y:xs)       = infixConvert remain (("(" ++ y ++ "*" ++ x ++ ")"):xs)
infixConvert (IntDiv: remain) ("0":y:xs) = Right DivByZero
infixConvert (IntDiv: remain) (x:y:xs)   = infixConvert remain (("(" ++ y ++ "/" ++ x ++ ")"):xs)
infixConvert _ _                         = Right InvalidInput
