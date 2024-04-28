module NExp where

data NExp = Var Variable | NCte Int | NBOp NBinOp NExp NExp 
data NBinOp = Add | Sub | Mul | Div | Mod | Pow
type Variable = String

data BExp = BCte Bool 
            | Not BExp  
            | And BExp BExp 
            | Or BExp BExp 
            | ROp RelOp NExp NExp

data RelOp = Eq | NEq -- Equal y NotEqual
           | Gt | GEq -- Greater y GreaterOrEqual
           | Lt | LEq -- Lower y LowerOrEqual

type Nombre = String

data Programa = Prog Bloque
type Bloque = [Comando]
data Comando = Assign Nombre NExp
             | If BExp Bloque Bloque
             | While BExp Bloque

instance Show NBinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show Pow = "^"

instance Show NExp where
  show (Var v) = v
  show (NCte n) = show n
  show (NBOp op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"

instance Show RelOp where
  show Eq = "=="
  show NEq = "!="
  show Gt = ">"
  show GEq = ">="
  show Lt = "<"
  show LEq = "<="

instance Show BExp where
  show (BCte b) = show b
  show (Not e) = "!" ++ show e
  show (And e1 e2) = "(" ++ show e1 ++ " && " ++ show e2 ++ ")"
  show (Or e1 e2) = "(" ++ show e1 ++ " || " ++ show e2 ++ ")"
  show (ROp rel ne1 ne2) = "(" ++ show ne1 ++ show rel ++ show ne2 ++ ")"

instance Show Programa where
  show (Prog b) = show b

instance Show Comando where
  show (Assign n e) = n ++ " = " ++ show e ++ "\n"
  show (If b bl1 bl2) = "if " ++ show b ++ " then " ++ show bl1 ++ " else " ++ show bl2 ++ "\n"
  show (While b bl) = "while " ++ show b ++ " do \n" ++ show bl ++ "\n"