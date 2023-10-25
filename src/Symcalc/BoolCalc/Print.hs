module Symcalc.BoolCalc.Print where

import Symcalc.BoolCalc.Eval
import Symcalc.Util.GenericExprPP

ppBinOp :: BinOp -> String
ppBinOp And = "&"
ppBinOp Or = "|"
ppBinOp Implies = "=>"
ppBinOp Equiv = "<=>"
ppBinOp Xor = "^"

operatorPrecedence :: PrecedenceTable BinOp
operatorPrecedence = mkPrecenceTable [[MkOp And DontCare], [MkOp Or DontCare, MkOp Xor DontCare], [MkOp Implies BRight, MkOp Equiv DontCare]]

exprToPP :: BoolExpr -> Expr BinOp
exprToPP (BSymbol s) = EAtom s
exprToPP (BValue v) = EAtom (show v)
exprToPP (BBinary op lhs rhs) = EBinary op (exprToPP lhs) (exprToPP rhs)
exprToPP (BUnary Negate inner) = EPrefix "!" (exprToPP inner)

ppExpr :: BoolExpr -> String
ppExpr = ppToString . addParentheses operatorPrecedence . exprToPP