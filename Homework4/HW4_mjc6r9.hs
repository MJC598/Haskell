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
eval []    = error "Bad Input"
eval p:ps  = if  

evalSafe :: PExp -> RPNResult
evalSafe = undefined

rpnTranse :: PExp -> String
rpnTranse = undefined