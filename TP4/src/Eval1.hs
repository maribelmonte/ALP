module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip               = return Skip
stepComm (Let v n)          = evalExp n >>= (update v) >>= (\_ -> return Skip)
stepComm (Seq Skip c)       = return c
stepComm (Seq c d)          = stepComm c >>= (\c' -> return (Seq c' d))
stepComm (IfThenElse b c d) = evalExp b >>= (\b' -> if b' then return c else return d)
stepComm (While b c)        = evalExp b >>= (\b' -> if b' then return (Seq c (While b c)) else return Skip)

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a

-- Expresiones enteras
evalExp (Const n)   = return n
evalExp (Var v)     = lookfor v
evalExp (UMinus n)  = evalExp n >>= (\n' -> return (- n'))
evalExp (Plus n m)  = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' + m')))
evalExp (Minus n m) = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' - m')))
evalExp (Times n m) = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' * m')))
evalExp (Div n m)   = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (div n' m')))

-- Expresiones booleanas
evalExp BTrue     = return True 
evalExp BFalse    = return False
evalExp (Lt n m)  = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' < m')))
evalExp (Gt n m)  = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' > m')))
evalExp (And b c) = evalExp b >>= (\b' -> evalExp c >>= (\c' -> return (b' && c')))
evalExp (Or b c)  = evalExp b >>= (\b' -> evalExp c >>= (\c' -> return (b' || c')))
evalExp (Not b)   = evalExp b >>= (\b' -> return (not b')) 
evalExp (Eq n m)  = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' == m')))
evalExp (NEq n m) = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' /=  m')))


