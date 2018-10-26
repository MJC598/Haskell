module HW4 where

--Due: Friday October 26, by 3pm

data Op   = Val Int | Plus | Minus | Mul | IntDiv deriving (Show, Eq)
type PExp = [Op] 

--PROBLEM 1

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

--PROBLEM 2
--Used a lot of concepts from the RPNCalculator in Learn You A Haskell For Great Good
--http://learnyouahaskell.com/functionally-solving-problems#reverse-polish-notation-calculator 

--evaluate the function after parsing
eval :: PExp -> Int
eval []  = error "Bad Input"
eval xs = intConverter(head(foldl foldingFunction [] xs))

foldingFunction :: PExp -> Op -> PExp
foldingFunction (x:y:ys) Plus   = (Val(intConverter(x) + intConverter(y))):ys
foldingFunction (x:y:ys) Minus  = (Val(intConverter(y) - intConverter(x))):ys
foldingFunction (x:y:ys) Mul    = (Val(intConverter(x) * intConverter(y))):ys
foldingFunction (x:y:ys) IntDiv = (Val(intConverter(y) `quot` intConverter(x))):ys
foldingFunction xs numberString = numberString:xs

--helper function for conversion purposes
intConverter :: Op -> Int
intConverter (Val x) = x 

--new data sets and types
data RPNError   = DivByZero | InvalidInput deriving (Show, Eq)
type RPNResult  = Either RPNError Int

--PROBLEM 3

evalSafe :: PExp -> RPNResult
evalSafe [] = Left InvalidInput
evalSafe xs = foldFunction xs []
--why can't we just build a handler to catch the error?

foldFunction :: PExp -> [Int] -> RPNResult
foldFunction [] (x:[])            = Right x
foldFunction ((Val x):ys) (xs)    = foldFunction (ys) (x:xs)
foldFunction (Plus:ys) (x:y:xs)   = foldFunction (ys)((x + y):xs)
foldFunction (Minus:ys) (x:y:xs)  = foldFunction (ys)((y - x):xs)
foldFunction (Mul:ys) (x:y:xs)    = foldFunction (ys)((x * y):xs) 
foldFunction (IntDiv:ys) (0:y:xs) = Left DivByZero
foldFunction (IntDiv:ys) (x:y:xs) = foldFunction (ys)((y `quot` x):xs)
foldFunction _ _                  = Left InvalidInput

--PROBLEM 4

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
--divide by zero case
infixConvert (IntDiv: remain) ("0":y:xs) = Right DivByZero
infixConvert (IntDiv: remain) (x:y:xs)   = infixConvert remain (("(" ++ y ++ "/" ++ x ++ ")"):xs)
--catch all for everything else
infixConvert _ _                         = Right InvalidInput
