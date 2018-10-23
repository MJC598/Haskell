module HW4 where

--Due: Friday October 26, by 3pm

data Op   = Val Int | Plus | Minus | Mul | IntDiv deriving (Show, Eq)
type PExp = [Op] 

rpnParse :: String -> PExp
rpnParse = undefined

eval :: PExp -> Int
eval = undefined

evalSafe :: PExp -> RPNResult
evalSafe = undefined

rpnTranse :: PExp -> String
rpnTranse = undefined