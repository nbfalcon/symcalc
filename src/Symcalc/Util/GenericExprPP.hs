module Symcalc.Util.GenericExprPP where

import Data.Map qualified as M
import Data.Maybe (fromJust)

data Expr binOp
    = EBinary binOp (Expr binOp) (Expr binOp)
    | EPrefix String (Expr binOp)
    | EParentheses (Expr binOp)
    | EAtom String

mapOp :: (binOp -> binOp2) -> Expr binOp -> Expr binOp2
mapOp f (EBinary op l r) = EBinary (f op) (mapOp f l) (mapOp f r)
mapOp f (EPrefix pfx inner) = EPrefix pfx (mapOp f inner)
mapOp f (EParentheses inner) = EParentheses $ mapOp f inner
mapOp _f (EAtom name) = EAtom name

-- Only care about binops
data Binding = BLeft | BRight | DontCare deriving (Eq) -- Equals is DontCare
data Op binOp = MkOp binOp Binding

data Operator binOp = Operator (Op binOp) Int
type PrecedenceTable binOp = M.Map binOp (Operator binOp)

mkPrecenceTable :: Ord binOp => [[Op binOp]] -> PrecedenceTable binOp
mkPrecenceTable precedenceTable = op2Prec
  where
    binOps = [(opId, Operator op prec) | (prec, ops) <- enumerate precedenceTable, op@(MkOp opId _) <- ops]
    op2Prec = M.fromList binOps

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

data BinExprSide = EBLeft | EBRight
-- Do we change binding sides? I.e. consider a right-associative (->) with a left-associative (+).
-- You would want to print (a + b) -> c as (a + b) -> c, and "a + b" needs to be in parentheses since the binding side changes between -> and (+).
isSideCrossToBinding :: Binding -> BinExprSide -> Bool
isSideCrossToBinding DontCare _ = False
isSideCrossToBinding BLeft EBLeft = False
isSideCrossToBinding BRight EBRight = False
isSideCrossToBinding _ _ = True

addParentheses1 :: Ord binOp => (Int, Binding, BinExprSide) -> PrecedenceTable binOp -> Expr binOp -> Expr binOp
addParentheses1 (previousPrecedence, previousBinding, side) precedenceTable inner@(EBinary op left right)
    | p > previousPrecedence || (p == previousPrecedence && crossBindingDirection) = EParentheses $ addParentheses precedenceTable inner
    | otherwise = EBinary op (addParentheses1 (p, myBinding, EBLeft) precedenceTable left) (addParentheses1 (p, myBinding, EBRight) precedenceTable right)
    where Operator (MkOp _ myBinding) p = fromJust $ M.lookup op precedenceTable
          -- If the precedence is the same, we normall don't want to add parens (a + b) + c -> a + b + c. However, if the precedence matters,
          -- We do want to add parentheses if we cross binding directions (see isSideCrossToBinding)
          crossBindingDirection = p == previousPrecedence && myBinding /= DontCare && isSideCrossToBinding previousBinding side
addParentheses1 _ precedenceTable e = addParentheses precedenceTable e 

addParentheses :: (Ord binOp) => PrecedenceTable binOp -> Expr binOp -> Expr binOp
addParentheses precedenceTable (EBinary op lhs rhs)
    = EBinary op newLeft newRight
    where
        newLeft = addParentheses1 (p, myBinding, EBLeft) precedenceTable lhs
        newRight = addParentheses1 (p, myBinding, EBRight) precedenceTable rhs
        Operator (MkOp _ myBinding) p = fromJust $ M.lookup op precedenceTable
addParentheses precedenceTable (EPrefix op inner@(EBinary {})) = EPrefix op (EParentheses $ addParentheses precedenceTable inner)
addParentheses _ other = other

-- Print an expression where parentheses were already added to a string
ppToString :: Show opId => Expr opId -> String
ppToString (EBinary op lhs rhs) = ppToString lhs ++ " " ++ show op ++ " " ++ ppToString rhs
ppToString (EAtom atom) = atom
ppToString (EPrefix pfx inner) = pfx ++ ppToString inner
ppToString (EParentheses inner) = "(" ++ ppToString inner ++ ")"