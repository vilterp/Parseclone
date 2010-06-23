module Parseclone.LexPrims (
  letter, digit, alphaNum, whitespace, stringLit, charLit, Lexer
) where

import Parseclone.Core
import Data.Char
import Data.List

type Lexer a = Parser Char a

-- lexer helper

letter :: Lexer Char
letter = literalP "letter" isAlpha

digit :: Lexer Char
digit = literalP "digit" isDigit

alphaNum :: Lexer Char
alphaNum = letter `eitherOr` digit

whitespace :: Lexer [Char]
whitespace = rep $ literalP "whitespace character" isSpace

stringLit :: Lexer String
stringLit = (literal '"') `andThen` (rep $ charVal True) `andThen` (literal '"') `wrap` \((_, cs), _) -> cs

charLit :: Lexer Char
charLit = (literal '\'') `andThen` (charVal False) `andThen` (literal '\'') `wrap` \((_, c), _) -> c

charVal :: Bool -> Lexer Char
charVal inString = (oneOf "printable ascii character" chars) `eitherOr` (escapedChar inString)
                   where chars = if inString
                                 then delete '"' allChars
                                 else delete '\'' allChars
                         allChars = delete '\\' [' '..'~']

escapedChar :: Bool -> Lexer Char
escapedChar inString = (literal '\\') `andThen` (oneOfP "escaped character" [newline, tab, backslash, quote]) `wrap` \(_, c) -> c
                       where quote = if inString then doubleQuote else singleQuote

newline = (literal 'n') `wrap` \_ -> '\n'

tab = (literal 't') `wrap` \_ -> '\t'

backslash = literal '\\'

singleQuote = literal '\''

doubleQuote = literal '\"'

