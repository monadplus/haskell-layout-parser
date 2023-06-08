module Main where

import Debug.Trace (traceM)
import Lexer
import Lexer.Support
import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  [filename] <- getArgs
  program <- readFile ("examples/" ++ filename ++ ".hs")
  print $ runLexer parseDecl program

lexAll :: Lexer ()
lexAll = do
  tok <- scan
  case tok of
    TkEOF -> pure ()
    x -> do
      traceM (show x)
      lexAll
