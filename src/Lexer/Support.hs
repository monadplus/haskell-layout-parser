{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lexer.Support where

import Control.Monad.Except
import Control.Monad.State
import Data.Char (ord)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Word

data Token
  = TkIdent String -- identifiers
  -- Keywords
  | TkLet
  | TkIn
  | TkWhere
  | -- Punctuation
    TkEqual
  | TkOpen
  | TkSemi
  | TkClose
  | TkLParen
  | TkRParen
  | TkBackslash
  | TkArrow
  | -- Layout punctuation
    TkVOpen
  | TkVSemi
  | TkVClose
  | -- End of file
    TkEOF
  deriving (Eq, Show)

data AlexInput = Input
  { inpLine :: {-# UNPACK #-} !Int,
    inpColumn :: {-# UNPACK #-} !Int,
    inpLast :: {-# UNPACK #-} !Char,
    inpStream :: String
  }
  deriving stock (Eq, Show)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte inp@Input {inpStream = str} = advance <$> List.uncons str
  where
    advance ('\n', rest) =
      ( fromIntegral (ord '\n'),
        Input
          { inpLine = inpLine inp + 1,
            inpColumn = 1,
            inpLast = '\n',
            inpStream = rest
          }
      )
    advance (c, rest) =
      ( fromIntegral (ord c),
        Input
          { inpLine = inpLine inp,
            inpColumn = inpColumn inp + 1,
            inpLast = c,
            inpStream = rest
          }
      )

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = inpLast

newtype Lexer a = Lexer {_getLexer :: StateT LexerState (Either String) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState LexerState,
      MonadError String
    )

newtype Layout = LayoutColumn Int
  deriving (Eq, Show, Ord)

data LexerState = LexerState
  { lexerInput :: {-# UNPACK #-} !AlexInput,
    lexerStartCodes :: {-# UNPACK #-} !(NonEmpty Int),
    lexerLayout :: [Layout]
  }
  deriving stock (Eq, Show)

initState :: String -> LexerState
initState str =
  LexerState
    { lexerInput = Input 0 1 '\n' str,
      lexerStartCodes = 0 :| [],
      lexerLayout = []
    }

runLexer :: Lexer a -> String -> Either String a
runLexer act s = evalStateT (_getLexer act) (initState s)

startCode :: Lexer Int
startCode = gets (NE.head . lexerStartCodes)

pushStartCode :: Int -> Lexer ()
pushStartCode i = modify' $ \st ->
  st
    { lexerStartCodes = NE.cons i (lexerStartCodes st)
    }

popStartCode :: Lexer ()
popStartCode = modify' $ \st ->
  st
    { lexerStartCodes =
        case lexerStartCodes st of
          _ :| [] -> 0 :| []
          _ :| (x : xs) -> x :| xs
    }

layout :: Lexer (Maybe Layout)
layout = gets (safeHead . lexerLayout)
  where
    safeHead [] = Nothing
    safeHead (x : _) = Just x

pushLayout :: Layout -> Lexer ()
pushLayout l = modify' $ \st ->
  st
    { lexerLayout = l : lexerLayout st
    }

popLayout :: Lexer ()
popLayout = modify' $ \st ->
  st
    { lexerLayout =
        case lexerLayout st of
          [] -> []
          (_ : xs) -> xs
    }

emit :: (String -> Token) -> String -> Lexer Token
emit = (pure .)

token :: Token -> String -> Lexer Token
token = const . pure
