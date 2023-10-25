module Symcalc.BoolCalc.Eval where

import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S

data BinOp = And | Or | Implies | Equiv | Xor deriving (Ord, Eq)
data UnaryOp = Negate deriving (Ord, Eq)
data BoolExpr
  = BSymbol String
  | BValue Bool
  | BBinary BinOp BoolExpr BoolExpr
  | BUnary UnaryOp BoolExpr
  deriving (Ord, Eq)

instance Show BinOp where
  show And = "&"
  show Or = "|"
  show Implies = "=>"
  show Equiv = "<=>"
  show Xor = "^"

instance Show BoolExpr where
  show (BSymbol s) = s
  show (BValue v) = show v
  show (BBinary op lhs rhs) = show lhs ++ " " ++ show op ++ " " ++ show rhs
  show (BUnary Negate inner) = "!" ++ show inner

evaluateBin :: BinOp -> Bool -> Bool -> Bool
evaluateBin And = (&&)
evaluateBin Or = (||)
evaluateBin Implies = \a b -> b || not a
evaluateBin Xor = (/=)
evaluateBin Equiv = (==)

evaluateUnary :: UnaryOp -> Bool -> Bool
evaluateUnary Negate = not

evaluate :: M.Map String Bool -> BoolExpr -> Bool
evaluate varMap (BSymbol symbol) = fromJust $ M.lookup symbol varMap
evaluate _ (BValue v) = v
evaluate varMap (BBinary binOp lhs rhs) = evaluateBin binOp (ev lhs) (ev rhs)
 where
  ev = evaluate varMap
evaluate varMap (BUnary unaryOp rhs) = evaluateUnary unaryOp (ev rhs)
 where
  ev = evaluate varMap

varsSeq :: BoolExpr -> [String]
varsSeq (BSymbol name) = [name]
varsSeq (BValue _) = []
varsSeq (BBinary _ lhs rhs) = varsSeq lhs ++ varsSeq rhs
varsSeq (BUnary _ inner) = varsSeq inner

varsOf :: BoolExpr -> S.Set String
varsOf = S.fromList . varsSeq

allPossibleInputs :: BoolExpr -> [M.Map String Bool]
allPossibleInputs expr = map M.fromAscList $ sequence possibilities
 where
  allVars = S.toAscList $ varsOf expr
  possibilities = [[(v, False), (v, True)] | v <- allVars]

subExpressions :: BoolExpr -> [BoolExpr]
subExpressions (BSymbol _) = [] -- symbols have to be printed separetly
subExpressions (BValue _) = []
subExpressions e@(BBinary _ lhs rhs) = subExpressions lhs ++ subExpressions rhs ++ [e]
subExpressions e@(BUnary _ inner) = subExpressions inner ++ [e]

data FullTableEvaluation = FullTableEvaluation {columns :: [BoolExpr], results :: [[Bool]]}
evaluateForTable :: BoolExpr -> FullTableEvaluation
evaluateForTable expr = FullTableEvaluation{columns, results = resultsTable}
 where
  evaluateExprs = subExpressions expr
  -- we want the vars to be sorted; looks bizarre otherwise
  forVars = S.toAscList $ varsOf expr
  columns = map BSymbol forVars ++ evaluateExprs
  resultsTable = [map (evaluate mapping) columns | mapping <- allPossibleInputs expr]

newtype AllEvaluations = AllEvaluations [Bool] deriving (Eq)
fullEval :: BoolExpr -> AllEvaluations
fullEval expr = AllEvaluations $ map (`evaluate` expr) $ allPossibleInputs expr

data Tautology = No | IsTautology | IsContradiction deriving (Eq, Show)
isTautology :: AllEvaluations -> Tautology
isTautology (AllEvaluations results)
  | allTrue = IsTautology
  | allFalse = IsContradiction
  | otherwise = No
 where
  allTrue = and results
  allFalse = all not results