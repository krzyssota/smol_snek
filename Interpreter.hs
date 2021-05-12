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
  evalStmt (VarDef pos idents expr) = do
    val <- evalExpr expr
    varsDecl' pos idents val
  evalStmt (FunDef pos ident args block) = do
    loc <- getNextLoc <$> get
    env <- asks $ M.insert ident loc
    insertValue loc (VFunc env args block)
    return env
  evalStmt (Print pos (e:exprs)) = printExpr e >> evalStmt (Print pos exprs)
  evalStmt _ = ask
 {-
  printExprs :: [Expr] -> InterpreterM Env
  printExprs [] = ask
  printExprs (e:exprs) = printExpr e >> printExprs exprs
  -}

  printExpr :: Expr -> InterpreterM ()
  printExpr e = do
    val <- evalExpr e
    liftIO (putStrLn (show val))
{-
  funDecl :: a -> Ident -> [Arg] -> Block -> InterpreterM Env
  funDecl pos ident args block = do
    loc <- getNextLoc <$> get
    env <- asks $ M.insert ident loc
    insertValue loc (VFunc env args block)
    return env


  varsDecl :: a -> [Ident] -> Expr -> InterpreterM Env
  varsDecl pos idents expr = do
    val <- evalExpr expr
    varsDecl' pos idents val
      -}

  varsDecl' :: a -> [Ident] -> Value -> InterpreterM Env
  varsDecl' pos [] val = ask
  varsDecl' pos (id:idents) val = do
    env <- varDecl pos id val
    local (const env) (varsDecl' pos idents val)

  varDecl :: a -> Ident -> Value -> InterpreterM Env
  varDecl pos ident val = do
    loc <- gets (getNextLoc)
    env <- asks (M.insert ident loc)
    insertValue loc val
    return env

  evalExpr :: Expr -> InterpreterM Value
  evalExpr (EStr pos s) = return (VString s)
  evalExpr (EInt pos i) = return (VInt i)
  evalExpr (ETrue pos) = return (VBool True)
  evalExpr (EFalse pos) = return (VBool False)
  evalExpr (EVar pos ident) = getVar ident
  evalExpr (ENot pos expr) = do
    val <- evalExpr expr
    return (VBool (not $ boolyVal val))
  evalExpr (ELog pos e1 opLog e2) = do -- and, or
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    let b1 = boolyVal v1; b2 = boolyVal v2 in
      case opLog of
        (And pos) -> return (VBool $ b1 && b2)
        (Or pos)  -> return (VBool $ b1 || b2)
  {-evalExpr (ECmp pos e1 opCmp e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    let b1 = boolyVal v1; b2 = boolyVal v2 in
      case opLog of
        (And pos) -> return b1 && b2
        (Or pos)  -> return b1 || b2-}
{-
TODO dalej cmp ari, potem stmt ifm sass pętle potem funkcje
  | ECall a Ident [Expr' a]
  | ETern a (Expr' a) (Expr' a) (Expr' a)
  | EAriUns a (Expr' a) (OpAriUns' a) (Expr' a)
  | EAriS a (Expr' a) (OpAriS' a) (Expr' a)
  | ECmp a (Expr' a) (OpCmp' a) (Expr' a)
  -}
  evalExpr _ = return VNull

  boolyVal :: Value -> Bool
  boolyVal val = case val of
                  VBool False -> True -- truthy
                  VInt 0 -> True
                  VString "" -> True
                  _ -> False          -- falsy

  arithyVal :: Value -> Maybe Integer
  arithyVal val = case val of
      VInt i -> Just i
      VBool True -> Just 1
      VBool False -> Just 0
      _ -> Nothing


  getVar :: Ident -> InterpreterM Value
  getVar ident = do
    loc <- getVarLoc ident
    getVarValue loc

  getVarLoc :: Ident -> InterpreterM Loc
  getVarLoc ident = do
    locMaybe <- asks (M.lookup ident)
    case locMaybe of
    --env <- ask                -- EQUIVALENT
    --case (M.lookup ident env)
      Nothing   -> throwError ("NameError: name " ++ show ident ++ " is not defined")
      Just loc -> return loc

  getVarValue :: Loc -> InterpreterM Value
  getVarValue loc = do
    valueMaybe <- gets (M.lookup loc)
    case valueMaybe of
      Nothing    -> throwError "hmmmmm sth went rly wrong, no value in location"
      Just value     -> return value

  getNextLoc :: Store -> Int
  getNextLoc s = M.size s + 1

  insertValue :: Loc -> Value -> InterpreterM ()
  insertValue loc val = do
    store' <- gets(M.insert loc val)
    put store'
    --s' <- M.insert loc val <$> get
    --put s'
