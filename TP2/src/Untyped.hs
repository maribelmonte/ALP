module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Sección 2 - Conversión a términos localmente sin nombres
-- Ejercicio 2
----------------------------------------------

conversion :: LamTerm -> Term
conversion t = conversion' t [] 

conversion' :: LamTerm -> [String] -> Term
conversion' (LVar x)  xs = maybe (Free (Global x)) (Bound) (elemIndex x xs)
conversion' (App t u) xs = (conversion' t xs) :@: (conversion' u xs)
conversion' (Abs x t) xs = Lam (conversion' t (x:xs))

-------------------------------
-- Sección 3 - Evaluación
-- Ejercicio 3 y 4
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f)     v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound i) (_, lEnv) = lEnv !! i
eval' (Free n)  (xs, ys)  = case funAux n xs of
                                 Just v  -> v
                                 Nothing -> VNeutral (NFree n)
eval' (t :@: u) (xs, ys)  = vapp (eval' t (xs, ys)) (eval' u (xs, ys))
eval' (Lam t)   (xs, ys)  = VLam (\v -> eval' t (xs, v:ys))

funAux :: Eq a => a -> [(a, b)] -> Maybe b
funAux a []     = Nothing
funAux a (x:xs) = if fst x == a then Just (snd x) else funAux a xs

--------------------------------
-- Sección 4 - Mostrando Valores
-- Ejercicio 5
--------------------------------

quote :: Value -> Term
quote v = replace (quote' v 0) 0

quote' :: Value -> Int -> Term
quote' (VLam f)              i = Lam (quote' (f (VNeutral (NFree (Quote i)))) (i+1))
quote' (VNeutral (NFree n))  i = Free n
quote' (VNeutral (NApp n v)) i = quote' (VNeutral n) i :@: quote' v i

replace :: Term -> Int -> Term
replace (Lam t)          i = Lam (replace t (i+1))
replace (Free (Quote k)) i = Bound (i-k-1)
replace (t :@: u)        i = replace t i :@: replace u i
replace t                _ = t