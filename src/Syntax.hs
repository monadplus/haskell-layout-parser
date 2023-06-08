module Syntax where

data Expr
  = Var String
  | App Expr Expr
  | Lam String Expr
  | Let [Decl] Expr
  deriving (Eq, Show)

data Decl = Decl
  { declName :: String,
    declRhs :: Expr,
    declWhere :: Maybe [Decl]
  }
  deriving (Eq, Show)

type Program = [Decl]
