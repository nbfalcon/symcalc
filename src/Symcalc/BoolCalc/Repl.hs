module Symcalc.BoolCalc.Repl (boolReplIO) where

import Control.Exception hiding (evaluate)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.State (MonadState (put), StateT, evalStateT)
import Control.Monad.State.Class (gets)
import Data.List (intercalate)
import Data.Text qualified as T
import GHC.IO.Exception
import Symcalc.BoolCalc.Eval
import Symcalc.BoolCalc.Parser
import Symcalc.BoolCalc.PrintLatex (ppExprLatex)
import Symcalc.BoolCalc.Print (ppExpr)
import System.IO
import System.IO.Error
import Text.Megaparsec (errorBundlePretty, parse)

data BoolReplState = BoolReplState {lastExpression :: Maybe BoolExpr}
newtype BoolReplMonad a = BoolReplMonad {runBoolRepl :: StateT BoolReplState IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadState BoolReplState)

read' :: IO (Maybe T.Text)
read' = do
    term <- hIsTerminalDevice stdin
    when term $ do
        putStr "Bool> "
        hFlush stdout
    nextLine <- tryIOError getLine
    case nextLine of
        Left IOError{ioe_type = EOF} -> pure Nothing
        Left anyOtherError -> throwIO anyOtherError
        Right text -> pure $ Just $ T.pack text

showB :: Bool -> String
showB False = "0"
showB True = "1"

evaluateInteractively :: BoolExpr -> BoolReplMonad ()
evaluateInteractively expr = do
    let eval = fullEval expr
    evalPrev <- (fullEval <$>) <$> gets lastExpression
    let tautology = isTautology eval
    let equalToLast = (eval ==) <$> evalPrev

    let FullTableEvaluation{columns, results} = evaluateForTable expr

    let tautologyString = case tautology of
            No -> "None"
            IsTautology -> "Tautology"
            IsContradiction -> "Contradiction"
    let equalsLastString = case equalToLast of
            Just False -> ", != last"
            Just True -> ", == last"
            Nothing -> ""
    let expressionHeader = ppExpr expr ++ ", " ++ tautologyString ++ equalsLastString
    let headerString = intercalate ", " $ map ppExpr columns
    let bodyString = concatMap ((++ "\n") . intercalate ", " . map showB) results
    liftIO $ do
        putStrLn expressionHeader
        putStrLn $ ppExprLatex expr
        putStrLn headerString
        putStrLn bodyString
    put BoolReplState{lastExpression = Just expr}

boolRepl :: BoolReplMonad ()
boolRepl = do
    exprS <- liftIO read'
    case exprS of
        Just exprSJ ->
            ( case parse boolExpr "<stdin>" exprSJ of
                Left err -> liftIO $ putStrLn (errorBundlePretty err)
                Right expr -> evaluateInteractively expr
            )
                >> boolRepl
        Nothing -> pure ()

boolReplIO :: IO ()
boolReplIO = flip evalStateT BoolReplState{lastExpression = Nothing} $ runBoolRepl boolRepl