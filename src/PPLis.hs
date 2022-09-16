{-# OPTIONS_GHC  -Wno-overlapping-patterns #-}

module PPLis where

import AST
import Text.PrettyPrint
import Prelude          hiding ( (<>) )

tabW :: Int
tabW = 2

pVar :: Variable -> Doc
pVar = text

pExp :: Exp a -> Doc
pExp (Const  i )   = int i
pExp (Var    x )   = pVar x
pExp (UMinus n )   = text "-" <+> pExp n
pExp (Plus  a b)   = pExp a <+> text "+" <+> pExp b
pExp (Times a b)   = pExp a <+> text "*" <+> pExp b
pExp (Minus a b)   = pExp a <+> text "-" <+> pExp b
pExp (Div   a b)   = pExp a <+> text "/" <+> pExp b
pExp BTrue         = text "true"
pExp BFalse        = text "false"
pExp (Eq  a b)     = pExp a <+> text "==" <+> pExp b
pExp (NEq a b)     = pExp a <+> text "!=" <+> pExp b
pExp (Lt  a b)     = pExp a <+> text "<" <+> pExp b
pExp (Gt  a b)     = pExp a <+> text ">" <+> pExp b
pExp (And a b)     = pExp a <+> text "&&" <+> pExp b
pExp (Or  a b)     = pExp a <+> text "||" <+> pExp b
pExp (Not b  )     = text "!" <+> pExp b
pExp (ECond b n m) = pExp b <+> text "?" <+> pExp n <+> text ":" <+> pExp m
pExp _ =
  error
    "El Pretty Printer no está implementado para esa extension."

pComm :: Comm -> Doc
pComm Skip         = text "skip"
pComm (Let x  e )  = pVar x <+> text "=" <+> pExp e
pComm (Seq c1 c2)  = pComm c1 <> semi $$ pComm c2
pComm (IfThen b c) =
  text "if" <+> parens (pExp b) <+> lbrace $$ nest tabW (pComm c) $$ rbrace
pComm (IfThenElse b c1 c2) =
  text "if"
    <+> parens (pExp b)
    <+> lbrace
    $$  nest tabW (pComm c1)
    $$  rbrace
    <+> text "else"
    <+> lbrace
    $$  nest tabW (pComm c2)
    $$  rbrace
pComm (While b c) =
  text "while" <+> parens (pExp b) $$ lbrace $$ nest tabW (pComm c) $$ rbrace
  
renderComm :: Comm -> String
renderComm = render . pComm

renderExp :: Exp a -> String
renderExp = render . pExp

