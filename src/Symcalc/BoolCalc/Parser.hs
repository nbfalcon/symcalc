{-# LANGUAGE OverloadedStrings #-}

module Symcalc.BoolCalc.Parser (boolExpr) where

import Control.Monad.Combinators.Expr
import Control.Monad.Combinators.Expr qualified as Expr
import Data.Text (Text)
import Data.Void (Void)
import Symcalc.BoolCalc.Eval (BinOp (And, Equiv, Implies, Or, Xor), BoolExpr (BBinary, BSymbol, BUnary, BValue), UnaryOp (Negate))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

spaceLitT :: Parser ()
spaceLitT = L.space hspace1 (L.skipLineComment "#" <|> L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

symbol = L.symbol spaceLitT
lexeme = L.lexeme spaceLitT

-- Parse a boolean variable or constant
boolValue :: Parser BoolExpr
boolValue =
    choice
        [ try $ BValue True <$ symbol "true"
        , try $ BValue False <$ symbol "false"
        , BSymbol <$> lexeme (some $ alphaNumChar <|> char '_')
        ]

-- Parse a boolean expression using operator precedence
boolExpr1 :: Parser BoolExpr
boolExpr1 = Expr.makeExprParser term operators

boolExpr :: Parser BoolExpr
boolExpr = spaceLitT >> boolExpr1 <* eof

term :: Parser BoolExpr
term =
    choice
        [ parens (lexeme boolExpr1)
        , boolValue
        ]

operators :: [[Expr.Operator Parser BoolExpr]]
operators =
    [ [Prefix (BUnary Negate <$ symbol "!")]
    , [InfixL (BBinary And <$ symbol "&"), InfixL (BBinary Xor <$ symbol "^")]
    , [InfixL (BBinary Or <$ symbol "|")]
    , [InfixR (BBinary Implies <$ try (symbol "=>")),
       InfixL (BBinary Equiv <$ symbol "<=>"),
       InfixL (BBinary Equiv <$ symbol "=")]
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")