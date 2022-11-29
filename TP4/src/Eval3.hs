module Eval3
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

-- Traza vacia
initTrace :: Trace
initTrace = ""

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 

-- Mónada estado, con manejo de errores y traza de ejecucion
newtype StateErrorTrace a =
  StateErrorTrace { runStateErrorTrace:: Env -> Either Error (Pair a (Pair Env Trace)) }

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

instance Monad StateErrorTrace where
  return x = StateErrorTrace (\s -> Right (x :!: (s :!: initTrace)))
  m >>= f  = StateErrorTrace (\s -> do (x :!: (s' :!: t)) <- runStateErrorTrace m s
                                       (x' :!: (s'' :!: t')) <- runStateErrorTrace (f x) s' 
                                       Right (x' :!: (s'' :!: t ++ t')))
-- Ejercicio 3.b: Resolver en Monad.hs

-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  addTrace t = StateErrorTrace (\s -> Right (() :!: (s :!: t)))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = StateErrorTrace (\_ -> Left e)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace (\s -> case M.lookup v s of
                                        Nothing -> Left UndefVar
                                        Just x  -> Right (x :!: (s :!: initTrace)))
  update v i = StateErrorTrace (\s -> Right (() :!: (M.insert v i s :!: initTrace)))

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error (Env, Trace)
eval p = case runStateErrorTrace (stepCommStar p) initEnv of
            Left e -> Left e
            Right (x :!: (s :!: t)) -> Right (s, t)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm Skip               = return Skip
stepComm (Let v n)          = evalExp n >>= (\n' -> update v n' >> 
                                                    addTrace ("Let " ++ v ++ " = " ++ show n' ++ ";\n ") >> 
                                                    return Skip)
stepComm (Seq Skip c)       = return c
stepComm (Seq c d)          = stepComm c >>= (\c' -> return (Seq c' d))
stepComm (IfThenElse b c d) = evalExp b >>= (\b' -> if b' then return c else return d)
stepComm (While b c)        = evalExp b >>= (\b' -> if b' then return (Seq c (While b c)) else return Skip)

-- Evalua una expresion 
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a

-- Expresiones enteras
evalExp (Const n)    = return n
evalExp (Var v)      = lookfor v
evalExp (UMinus n)   = evalExp n >>= (\n' -> return (- n'))
evalExp (Plus n m)   = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' + m')))
evalExp (Minus n m)  = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' - m')))
evalExp (Times n m)  = evalExp n >>= (\n' -> evalExp m >>= (\m' -> return (n' * m')))
evalExp (Div n m)    = evalExp n >>= (\n' -> evalExp m >>= (\m' -> if m' == 0 then throw DivByZero 
                                                                              else return (div n' m')))
evalExp (EAssgn v n) = evalExp n >>= (\n' -> update v n' >> return n')
evalExp (ESeq n m)   = evalExp n >> evalExp m

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