module Symcalc.BoolCalc.PrintLatex where

import Symcalc.BoolCalc.Eval
import Symcalc.BoolCalc.Print (operatorPrecedence)
import Symcalc.Util.GenericExprPP

newtype LatexBinop = LatexBinop BinOp
instance Show LatexBinop where
    show (LatexBinop And) = "\\wedge"
    show (LatexBinop Or) = "\\vee"
    show (LatexBinop Implies) = "\\rightarrow"
    show (LatexBinop Equiv) = "\\leftrightarrow"
    show (LatexBinop Xor) = "\\oplus"

exprToPPLatex :: BoolExpr -> Expr BinOp
exprToPPLatex (BSymbol s) = EAtom s
exprToPPLatex (BValue v) = EAtom (show v)
exprToPPLatex (BBinary op lhs rhs) = EBinary op (exprToPPLatex lhs) (exprToPPLatex rhs)
exprToPPLatex (BUnary Negate inner) = EPrefix "\\neg " (exprToPPLatex inner)

ppExprLatex :: BoolExpr -> String
ppExprLatex = ppToString . mapOp LatexBinop . addParentheses operatorPrecedence . exprToPPLatex