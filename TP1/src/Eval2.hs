module Eval2
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
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v s of 
                   Just n  -> Right n
                   Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update k v s = M.insert k v s

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do (c' :!: s') <- stepComm c s
                         stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip               s = Right (Skip :!: s)
stepComm (Let v n)          s = do r <- evalExp n s
                                   Right (Skip :!: update v r s)
stepComm (Seq Skip c)       s = Right (c :!: s)
stepComm (Seq c d)          s = do (c' :!: s') <- stepComm c s 
                                   Right (Seq c' d :!: s') 
stepComm (IfThenElse b c d) s = do r <- evalExp b s
                                   if r then Right (c :!: s) 
                                        else Right (d :!: s)
stepComm (While b c)        s = do r <- evalExp b s
                                   if r then Right (Seq c (While b c) :!: s)
                                        else Right (Skip :!: s)

-- Evalua una expresion
evalExp :: Exp a -> State -> Either Error a

-- Expresiones enteras
evalExp (Const n)     s = Right n
evalExp (Var v)       s = lookfor v s
evalExp (UMinus n)    s = do r <- evalExp n s
                             Right (-r)
evalExp (Plus n m)    s = funAux (+) n m s
evalExp (Minus n m)   s = funAux (-) n m s
evalExp (Times n m)   s = funAux (*) n m s
evalExp (Div n m)     s = do n' <- evalExp n s
                             m' <- evalExp m s
                             if m' == 0 then Left DivByZero 
                                        else Right (div n' m')
evalExp (ECond b n m) s = do b' <- evalExp b s
                             if b' then evalExp n s else evalExp m s

-- Expresiones booleanas
evalExp BTrue     s = Right True
evalExp BFalse    s = Right False
evalExp (Lt n m)  s = funAux (<)  n m s
evalExp (Gt n m)  s = funAux (>)  n m s
evalExp (And b c) s = funAux (&&) b c s
evalExp (Or b c)  s = funAux (||) b c s
evalExp (Not b)   s = do r <- evalExp b s
                         Right (not r) 
evalExp (Eq n m)  s = funAux (==) n m s
evalExp (NEq n m) s = funAux (/=) n m s


-- Funcion auxiliar                       
funAux op n m s = do n' <- evalExp n s
                     m' <- evalExp m s
                     Right (op n' m')