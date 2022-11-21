module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> runStateError m s >>= (\(x :!: s') -> runStateError (f x) s'))

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw e = StateError (\_ -> Left e)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> case M.lookup v s of
                                    Nothing -> Left UndefVar
                                    Just x  -> Right (x :!: s))
  update v i = StateError (\s -> Right (() :!: M.insert v i s))

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = case runStateError (stepCommStar p) initEnv of
             Left e -> Left e
             Right (x :!: s) -> Right s

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm Skip               = return Skip
stepComm (Let v n)          = evalExp n >>= (update v) >> return Skip
stepComm (Seq Skip c)       = return c
stepComm (Seq c d)          = stepComm c >>= (\c' -> return (Seq c' d))
stepComm (IfThenElse b c d) = evalExp b >>= (\b' -> if b' then return c else return d)
stepComm (While b c)        = evalExp b >>= (\b' -> if b' then return (Seq c (While b c)) else return Skip)

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m) => Exp a -> m a

-- Expresiones enteras
evalExp (Const n)   = return n
evalExp (Var v)     = lookfor v
evalExp (UMinus n)  = evalExp n >>= (\n' -> return (- n'))
evalExp (Plus n m)  = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' + m')))
evalExp (Minus n m) = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' - m')))
evalExp (Times n m) = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' * m')))
evalExp (Div n m)   = evalExp n >>= (\n' -> evalExp m >>= (\m' -> if m' == 0 then throw DivByZero 
                                                                             else return (div n' m')))

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


