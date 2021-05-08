module Interpreter where
  import Control.Monad.IO.Class
  import Control.Monad.State
  import Control.Monad.Reader
  import Control.Monad.Error
  import Debug.Trace
  import System.IO
  import Types
  import AbsSmolSnekGrammar
  import LexSmolSnekGrammar
  import ParSmolSnekGrammar
  import SkelSmolSnekGrammar
  import ErrM
  import PrintSmolSnekGrammar

  import qualified Data.Map.Lazy as M

  runInterpreterM :: Env -> Store -> InterpreterM a -> IO (Either String a, Store)
  runInterpreterM env store im = runStateT (runErrorT (runReaderT im env)) store

  runProgram :: Program -> InterpreterM Integer
  runProgram (Program stmts positions) = do
    --env <- interStatements stmts
    --VInt val <- local (const env) $ evalExpr $ EApp (Ident "main") []
    return 0
{-
  evalStmts :: [Stmt'] -> InterpreterM Integer
  evalStmts [] = ask -- returns the existing env ??
  evalStmts (Expr e:ss) = do
    env <- evalStmt s
    local (const env) (evalStmts ss)

  evalExpr :: Stmt' -> InterpreterM Value
  -}
