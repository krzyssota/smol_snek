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
  runProgram (Program pos stmts) = do
    env <- evalStmts stmts
    --VInt val <- local (const env) $ evalExpr $ EApp (Ident "main") []
    liftIO $ putStrLn $ show env
    store <- get
    liftIO $ putStrLn $ show store
    return 0

  evalStmts :: [Stmt] -> InterpreterM Env
  evalStmts [] = ask -- returns the existing env ??
  evalStmts (s:ss) = do
    env <- evalStmt s
    local (const env) (evalStmts ss)

  evalStmt :: Stmt -> InterpreterM Env
  evalStmt (VarDef pos idents expr) = varsDecl (VarDef pos idents expr)
  evalStmt (FunDef pos ident args block) = funDecl (FunDef pos ident args block)
  evalStmt _ = ask

  funDecl :: Stmt -> InterpreterM Env
  funDecl (FunDef pos ident args block) = do
    loc <- getNextLoc <$> get
    env <- asks $ M.insert ident loc
    insertValueStore loc (VFunc env args block)
    return env

  varsDecl :: Stmt -> InterpreterM Env -- TODO optimize eval Expr once
  varsDecl (VarDef pos [] expr) = ask
  varsDecl (VarDef pos (id:idents) expr) = do
    env <- varDecl (VarDef pos [id] expr)
    local (const env) (varsDecl (VarDef pos idents expr))

  varDecl :: Stmt -> InterpreterM Env
  varDecl (VarDef pos [] expr) = ask
  varDecl (VarDef pos (ident:_) expr) = do
    loc <- getNextLoc <$> get
    env <- asks $ M.insert ident loc
    insertValueStore loc VNull -- TODO insert evaled expr
    return env
  {-
  varDecl :: Stmt -> InterpreterM Env
  varDecl (VarDef pos idents expr) = do
    loc <- getNextLoc <$> get
    env <- asks $ M.insert idents loc
    insertValueStore loc VNull -- TODO insert evaled expr
    return env
    -}

  getNextLoc :: Store -> Int
  getNextLoc s = M.size s + 1

  insertValueStore :: Loc -> Value -> InterpreterM ()
  insertValueStore loc val = do
    s' <- M.insert loc val <$> get
    put s'
