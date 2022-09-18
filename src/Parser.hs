module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language           ( emptyDef )
import AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do whiteSpace lis
                 t <- p
                 eof
                 return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "while", "skip" , "do"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "{"
                        , "}"
                        , "?"
                        , ":"
                        ]
    }
  )


----------------------------------
--- Parser de expressiones enteras
-----------------------------------

intexp :: Parser (Exp Int)
intexp = term `chainl1` plusOrminus <|> ternario

term = factor `chainl1` timesOrdiv

factor = try (parens lis intexp) <|> cons <|> var <|> uminus 

cons = do n <- natural lis
          return (Const (fromIntegral n))

var = do v <- identifier lis
         return (Var v)  

uminus = do reservedOp lis "-"
            do n <- factor
               return (UMinus n) 
             <|> do n <- intexp 
                    return (UMinus n) 

plusOrminus = do reservedOp lis "+"
                 return Plus 
              <|> do reservedOp lis "-"
                     return Minus 
                
timesOrdiv = do reservedOp lis "*"
                return Times
             <|> do reservedOp lis "/"
                    return Div

ternario = do b <- boolexp
              reservedOp lis "?"
              n <- intexp
              reservedOp lis ":"
              m <- intexp
              return (ECond b n m)


-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = bterm `chainl1` orOp

bterm = bfactor `chainl1` andOp

bfactor = (parens lis boolexp) <|> trueOrfalse <|> negation <|> compareOp

trueOrfalse = do reserved lis "true"
                 return BTrue
              <|> do reserved lis "false"
                     return BFalse

negation = do reservedOp lis "!"
              do b <- bfactor
                 return (Not b)
               <|> do b <- boolexp 
                      return (Not b)
              

andOp = do reservedOp lis "&&"
           return And

orOp = do reservedOp lis "||"
          return Or
                  
compareOp = do n <- intexp
               do reservedOp lis "<"
                  m <- intexp
                  return (Lt n m)
                <|> do reservedOp lis ">"
                       m <- intexp
                       return (Gt n m)
                <|> do reservedOp lis "=="
                       m <- intexp
                       return (Eq n m)
                <|> do reservedOp lis "!="
                       m <- intexp
                       return (NEq n m)


-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm 
comm = (skip <|> assign <|> ifthenelse <|> while) `chainl1` sequen

skip = do reserved lis "skip"
          return Skip

assign = do v <- identifier lis
            reservedOp lis "="
            n <- intexp
            return (Let v n)

sequen = do reservedOp lis ";"
            return Seq

ifthenelse = do reserved lis "if"
                b <- boolexp
                reservedOp lis "{"
                c <- comm
                reservedOp lis "}"
                do reserved lis "else"
                   reservedOp lis "{"
                   d <- comm
                   reservedOp lis "}"
                   return (IfThenElse b c d)
                 <|> return (IfThen b c)

while = do reserved lis "while"
           b <- boolexp
           reservedOp lis "{"
           c <- comm
           reservedOp lis "}"
           return (While b c)


------------------------------------
-- Función de parseo
------------------------------------

parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)