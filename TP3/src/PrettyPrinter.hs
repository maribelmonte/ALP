module PrettyPrinter
  ( printTerm
  ,     -- pretty printer para terminos
    printType     -- pretty printer para tipos
  )
where

import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )
-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k)          = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c) = sep
  [ parensIf (isLam i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c

pp ii vs (Let t u ) =
  text ("let ")
    <> pp ii vs t
    <> text " in "
    <> pp ii vs u

pp ii vs (As s t) =
  pp ii vs s
    <> text " as "
    <> printType t

pp ii vs (Unit) =
  text "unit"

pp ii vs (Fst t) =
  text "fst "
    <> pp ii vs t

pp ii vs (Snd t) =
  text "snd "
    <> pp ii vs t

pp ii vs (Pair t u) =
  parens
    (pp ii vs t
    <> text ", "
    <> pp ii vs u)

pp ii vs (Zero) =
  text "0"

pp ii vs (Suc t) =
  text "suc "
    <> pp ii vs t

pp ii vs (Rec t u v) =
  text "R "
    <> parens (pp ii vs t)
    <> parens (pp ii vs u)
    <> parens (pp ii vs v)

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
printType UnitT = text "Unit"
printType (PairT t1 t2) = 
  parens
    (printType t1
    <> text ", "
    <> printType t2)
printType (NatT) = text "Nat"
  


isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _)          = []
fv (Free  (Global n)) = [n]
fv (t :@: u)          = fv t ++ fv u
fv (Lam _   u)        = fv u
fv (Let t u)          = fv t ++ fv u
fv (As s t)           = fv s
fv (Unit)             = []
fv (Fst t)            = fv t
fv (Snd t)            = fv t
fv (Pair t u)         = fv t ++ fv u
fv (Zero)             = []
fv (Suc t)            = fv t 
fv (Rec t u v)        = fv t ++ fv u ++ fv v

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

