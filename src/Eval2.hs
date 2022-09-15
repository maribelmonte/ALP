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
lookfor v s = case (M.lookup v s) of 
                   (Just n) -> Right n
                   Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update k v s = M.adjust (\x -> v) k s

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
                                   Right (Skip :!: (M.insert v r s))
stepComm (Seq Skip c)       s = Right (c :!: s)
stepComm (Seq c d)          s = do r <- stepComm c s 
                                   Right ((Seq (T.fst r) d) :!: T.snd r) 
stepComm (IfThenElse b c d) s = do r <- evalExp b s
                                   Right (if r then (c :!: s) else (d :!: s))
stepComm (While b c)        s = do r <- evalExp b s
                                   Right (if r then (Seq c (While b c) :!: s) else (Skip :!: s))

-- Funcion auxilar
op f (Right n) (Right m) = Right (f n m)
op f (Left e) _ = Left e
op f _ (Left e) = Left e

-- Evalua una expresion

-- Expresiones enteras
evalExp :: Exp a -> State -> Either Error a 
evalExp (Const n)     s = Right n
evalExp (Var v)       s = lookfor v s
evalExp (UMinus n)    s = do r <- evalExp n s
                             Right (-r)
evalExp (Plus n m)    s = op (+) (evalExp n s) (evalExp m s)
evalExp (Minus n m)   s = op (-) (evalExp n s) (evalExp m s)
evalExp (Times n m)   s = op (*) (evalExp n s) (evalExp m s)
evalExp (Div n m)     s = do n' <- (evalExp n s) 
                             m' <- (evalExp m s)
                             if m' == 0 then Left DivByZero else Right (div n' m')
evalExp (ECond b n m) s = do b' <- (evalExp b s)
                             if b' then (evalExp n s) else (evalExp m s)

-- Expresiones booleanas
evalExp BTrue     s = Right True 
evalExp BFalse    s = Right False
evalExp (Lt n m)  s = op (<)  (evalExp n s) (evalExp m s)
evalExp (Gt n m)  s = op (>)  (evalExp n s) (evalExp m s)
evalExp (And b c) s = op (&&) (evalExp b s) (evalExp c s)
evalExp (Or b c)  s = op (||) (evalExp b s) (evalExp c s)
evalExp (Not b)   s = do r <- evalExp b s
                         Right (not r) 
evalExp (Eq n m)  s = op (==) (evalExp n s) (evalExp m s)
evalExp (NEq n m) s = op (/=) (evalExp n s) (evalExp m s)
