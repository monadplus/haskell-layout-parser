{
module Parser where

import Lexer.Support
import Control.Monad.Except
import Lexer
import Syntax
}

%name parseExpr Expr
%name parseDecl Decl

%tokentype { Token }
%monad { Lexer }
%lexer { lexer } { TkEOF }

%errorhandlertype explist
%error { parseError }

%token
  VAR     { TkIdent $$ }
  'let'   { TkLet }
  'in'    { TkIn }
  'where' { TkWhere }

  '='     { TkEqual }
  '{'     { TkOpen }
  ';'     { TkSemi }
  '}'     { TkClose }
  '\\'    { TkBackslash }
  '->'    { TkArrow }

  '('     { TkLParen }
  ')'     { TkRParen }

  OPEN    { TkVOpen }
  SEMI    { TkVSemi }
  CLOSE   { TkVClose }

%%

Atom :: { Expr }
  : VAR          { Var $1 }
  | '(' Expr ')' { $2 }

Expr :: { Expr }
  : '\\' VAR '->' Expr { Lam $2 $4 }
  | FuncExpr           { $1 }
  | 'let' DeclBlock 'in' Expr { Let $2 $4 }

FuncExpr :: { Expr }
  : FuncExpr Atom { App $1 $2 }
  | Atom          { $1 }

DeclBlock :: { [Decl] }
  : '{' DeclListSemi '}'    { $2 }
  | OPEN DeclListSEMI Close { $2 }

DeclListSemi :: { [Decl] }
  : Decl ';' DeclListSemi { $1:$3 }
  | Decl                  { [$1] }
  | {- empty -}           { [] }

DeclListSEMI :: { [Decl] }
  : Decl SEMI DeclListSemi { $1:$3 }
  | Decl                   { [$1] }
  | {- empty -}            { [] }

Close
  : CLOSE { () }
  | error {% popLayout }

Decl
  : VAR '=' Expr { Decl $1 $3 Nothing }
  | VAR '=' Expr 'where' DeclBlock { Decl $1 $3 (Just $5) }

{
lexer cont = scan >>= cont

parseError = throwError . show
}
