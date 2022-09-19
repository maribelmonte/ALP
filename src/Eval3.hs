module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict   as M
import           Data.Strict.Tuple as T
import           Prelude           as P

-- Estados 
type State = (M.Map Variable Int, Integer)

-- Estado nulo
initState :: State
initState = (M.empty, 0)

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v (s,w) = case M.lookup v s of 
                       Just n  -> Right n
                       Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update k v (s,w) = (M.insert k v s, w)

-- Suma un costo dado al estado
addWork :: Integer -> State -> State
addWork w' (s,w) = (s, w+w')

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
stepComm (Let v n)          s = do (r :!: s') <- evalExp n s
                                   Right (Skip :!: update v r s')
stepComm (Seq Skip c)       s = Right (c :!: s)
stepComm (Seq c d)          s = do (c' :!: s') <- stepComm c s
                                   Right ((Seq c' d) :!: s')
stepComm (IfThenElse b c d) s = do (r :!: s') <- evalExp b s
                                   if r then Right (c :!: s') 
                                        else Right (d :!: s')
stepComm (While b c)        s = do (r :!: s') <- evalExp b s 
                                   if r then Right (Seq c (While b c) :!: s') 
                                        else Right (Skip :!: s')

-- Evalua una expresion
evalExp :: Exp a -> State -> Either Error (Pair a State)

-- Expresiones enteras
evalExp (Const n)     s = Right (n :!: s)
evalExp (Var v)       s = do r <- lookfor v s
                             Right (r :!: s)
evalExp (UMinus n)    s = do (r :!: s') <- evalExp n s
                             Right ((-r) :!: addWork 1 s')
evalExp (Plus n m)    s = funAux (+) n m s 2
evalExp (Minus n m)   s = funAux (-) n m s 2
evalExp (Times n m)   s = funAux (*) n m s 3
evalExp (Div n m)     s = do (n' :!: s1) <- evalExp n s
                             (m' :!: s2) <- evalExp m s1
                             if m' == 0 then Left DivByZero 
                                        else Right ((div n' m') :!: addWork 3 s2)
evalExp (ECond b n m) s = do (b' :!: s') <- evalExp b s
                             if b' then evalExp n s' else evalExp m s'

-- Expresiones booleanas
evalExp BTrue     s = Right (True :!: s) 
evalExp BFalse    s = Right (False :!: s)
evalExp (Lt n m)  s = funAux (<)  n m s 2
evalExp (Gt n m)  s = funAux (>)  n m s 2
evalExp (And b c) s = funAux (&&) b c s 2
evalExp (Or b c)  s = funAux (||) b c s 2
evalExp (Not b)   s = do (r :!: s') <- evalExp b s
                         Right ((not r) :!: addWork 1 s')
evalExp (Eq n m)  s = funAux (==) n m s 2
evalExp (NEq n m) s = funAux (/=) n m s 2


-- Funcion auxiliar                       
funAux op n m s w = do (n' :!: s1) <- evalExp n s
                       (m' :!: s2) <- evalExp m s1
                       Right ((op n' m') :!: addWork w s2)