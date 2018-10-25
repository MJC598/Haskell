module HW4 where

--Due: Friday October 26, by 3pm

data Op   = Val Int | Plus | Minus | Mul | IntDiv deriving (Show, Eq)
type PExp = [Op] 

rpnParse :: String -> PExp
rpnParse st = map(tester s) (words(st))
    where tester :: String -> Op
          tester "+" = Plus
          tester "-" = Minus
          tester "*" = Mul
          tester "/" = IntDiv
          tester s   = Val read s

eval :: PExp -> Int
eval []  = error "Bad Input"
eval ps  = head . foldl folding []
        where folding    :: PExp -> Op -> PExp 
              folding (x:y:ps) Plus   = (x + y):ys 
              folding (x:y:ps) Minus  = (x - y):ys
              folding (x:y:ps) Mul    = (x ** y):ys
              folding (x:y:ps) IntDiv = (x / y):ys
              folding ps Val Int      = p:ps 

evalSafe :: PExp -> RPNResult
evalSafe = undefined

rpnTranse :: PExp -> String
rpnTranse = undefined