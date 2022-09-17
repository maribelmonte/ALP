module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict   as M
import           Data.Strict.Tuple as T

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update k v s = M.adjust (\x -> v) k s

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = T.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip               s = (Skip :!: s)
stepComm (Let v n)          s = (Skip :!: r) where r = M.insert v (evalExp n s) s
stepComm (Seq Skip c)       s = (c :!: s)
stepComm (Seq c d)          s = ((Seq (T.fst r) d) :!: T.snd r) where r = stepComm c s
stepComm (IfThenElse b c d) s = if evalExp b s then (c :!: s) 
                                               else (d :!: s)
stepComm (While b c)        s = if evalExp b s then (Seq c (While b c) :!: s) 
                                               else (Skip :!: s) 

-- Evalua una expresion

-- Expresiones enteras
evalExp :: Exp a -> State -> a 
evalExp (Const n)     s = n
evalExp (Var v)       s = lookfor v s
evalExp (UMinus n)    s = - (evalExp n s)
evalExp (Plus n m)    s = (evalExp n s) + (evalExp m s)
evalExp (Minus n m)   s = (evalExp n s) - (evalExp m s)
evalExp (Times n m)   s = (evalExp n s) * (evalExp m s)
evalExp (Div n m)     s = div (evalExp n s) (evalExp m s)
evalExp (ECond b n m) s = if evalExp b s then evalExp n s else evalExp m s

-- Expresiones booleanas
evalExp BTrue     s = True 
evalExp BFalse    s = False
evalExp (Lt n m)  s = (evalExp n s) <  (evalExp m s)
evalExp (Gt n m)  s = (evalExp n s) >  (evalExp m s)
evalExp (And b c) s = (evalExp b s) && (evalExp c s)
evalExp (Or b c)  s = (evalExp b s) || (evalExp c s)
evalExp (Not b)   s = not (evalExp b s) 
evalExp (Eq n m)  s = (evalExp n s) == (evalExp m s)
evalExp (NEq n m) s = (evalExp n s) /= (evalExp m s)