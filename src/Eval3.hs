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
lookfor v (s,w) = case (M.lookup v s) of 
                       (Just n) -> Right n
                       Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update k v (s,w) = (M.adjust (\x -> v) k s, w)

-- Suma un costo dado al estado
addWork :: Integer -> State -> State
addWork w' (s,w) = (s,w+w')

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
stepComm (Let v n)          s = do (r, w) <- (evalExp n s)
                                   Right (Skip :!: ((M.insert v r (P.fst s)), w))
stepComm (Seq Skip c)       s = Right (c :!: s)
stepComm (Seq c d)          s = do r <- stepComm c s
                                   Right ((Seq (T.fst r) d) :!: T.snd r)
stepComm (IfThenElse b c d) s = do (r, w) <- (evalExp b s)
                                   if r then Right (c :!: (P.fst s, w)) 
                                        else Right (d :!: (P.fst s, w))
stepComm (While b c)        s = do (r,w) <- (evalExp b s) 
                                   if r then Right (Seq c (While b c) :!: (P.fst s, w)) 
                                        else Right (Skip :!: (P.fst s, w))

-- Funciones auxilar
op f (Right (n,w1)) (Right (m,w2)) s = Right ((f n m), w1+w2-P.snd s)
op f (Left e)       _              s = Left e
op f _              (Left e)       s = Left e

-- Evalua una expresion

-- Expresiones enteras
evalExp :: Exp a -> State -> Either Error (a, Integer) 
evalExp (Const n)     s = Right (n, P.snd s)
evalExp (Var v)       s = do r <- lookfor v s
                             Right (r, P.snd s)
evalExp (UMinus n)    s = do (r,w') <- (evalExp n s)
                             Right (-r, w'+1)
evalExp (Plus n m)    s = op (+) (evalExp n s) (evalExp m s) (addWork (-2) s)
evalExp (Minus n m)   s = op (-) (evalExp n s) (evalExp m s) (addWork (-2) s)
evalExp (Times n m)   s = op (*) (evalExp n s) (evalExp m s) (addWork (-3) s)
evalExp (Div n m)     s = do (n', w1) <- (evalExp n s) 
                             (m', w2) <- (evalExp m s)
                             if m' == 0 then Left DivByZero 
                                        else Right ((div n' m'), 3+w1+w2-P.snd s)
evalExp (ECond b n m) s = do (b', w) <- (evalExp b s)
                             if b' then (evalExp n (P.fst s,w)) else (evalExp m (P.fst s,w))
                             

-- Expresiones booleanas
evalExp BTrue     s = Right (True, P.snd s) 
evalExp BFalse    s = Right (False, P.snd s)
evalExp (Lt n m)  s = op (<)  (evalExp n s) (evalExp m s) (addWork (-2) s)
evalExp (Gt n m)  s = op (>)  (evalExp n s) (evalExp m s) (addWork (-2) s)
evalExp (And b c) s = op (&&) (evalExp b s) (evalExp c s) (addWork (-2) s)
evalExp (Or b c)  s = op (||) (evalExp b s) (evalExp c s) (addWork (-2) s)
evalExp (Not b)   s = do (r, w') <- (evalExp b s)
                         Right ((not r),w'+1)
evalExp (Eq n m)  s = op (==) (evalExp n s) (evalExp m s) (addWork (-2) s)
evalExp (NEq n m) s = op (/=) (evalExp n s) (evalExp m s) (addWork (-2) s)


