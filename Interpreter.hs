{-# LANGUAGE NoMonomorphismRestriction #-}

module Interpreter where
  import Control.Monad.IO.Class
  import Control.Monad.State
  import Control.Monad.Reader
  import Control.Monad.Error
  import Debug.Trace
  import System.IO
  import Types
  import Error
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
    res <- execStmts stmts
    case res of
      ReturnVal val -> liftIO $ putStrLn $ "retval " ++ show val
      ReturnEnv env -> liftIO $ putStrLn $ "retenv " ++ show env
      bc            -> liftIO $ putStrLn $ show bc -- TODO tutaj error
    store <- get
    liftIO $ putStrLn $ show store
    return 0

  execBlock :: Block -> InterpreterM StmtResult -- TODO może zmergować execBlock/Stmts
  execBlock (Block pos stmts) = execStmts stmts

  execStmts :: [Stmt] -> InterpreterM StmtResult
  execStmts [] = do
    env <- ask
    return (ReturnEnv env)
  execStmts (s:ss) = do
    res <- execStmt s
    case res of
      (ReturnEnv env) -> do
        local (const env) (execStmts ss)
      (ReturnVal val) -> do
        return (ReturnVal val)
      _ -> throwError $ showSyntaxError res

  execStmt :: Stmt -> InterpreterM StmtResult
  execStmt (VarDef pos idents expr) = do
    val <- evalExpr expr
    varsDecl' pos idents val
  execStmt (FunDef pos ident args block) = do
    loc <- getNextLoc <$> get
    env <- asks $ M.insert ident loc
    insertValue loc (VFunc env args block)
    return (ReturnEnv env)
  execStmt (Print pos []) = do
    liftIO (putStrLn "")
    retenv
    --env <- ask
    --return (ReturnEnv env)
  execStmt (Print pos (e:exprs)) = printExpr e >> liftIO (putStr " ") >> execStmt (Print pos exprs)
  execStmt (SBlock pos block) = execBlock block
  execStmt (SIf pos expr stmt) = do
    val <- evalExpr expr
    if boolyVal val
      then execStmt stmt
      else retenv
  execStmt (SIfElse pos expr ifStmt (SElse pos2 elseStmt)) = do
    val <- evalExpr expr
    if boolyVal val
      then execStmt ifStmt
      else execStmt elseStmt
  execStmt (SIfElif pos expr ifStmt elif) = do
    val <- evalExpr expr
    if boolyVal val
      then execStmt ifStmt
      else execElif elif
  execStmt while@(SWhile pos expr lblock) = do
    val <- evalExpr expr
    if boolyVal val
      then do
        res <- execLoopBlock lblock
        case res of
          Cont      -> execStmt while
          Break         -> retenv
          ReturnVal val -> return (ReturnVal val)
          ReturnEnv env -> execStmt while
      else retenv
  -- TODO usunac zeby wiedziec ze wszyskto jest pokryte
  execStmt _ = do
    env <- ask
    return (ReturnEnv env)

  execLoopBlock :: LoopBlock -> InterpreterM StmtResult -- TODO maybe merge
  execLoopBlock (LBlock pos loopStmts) = execLoopStmts loopStmts
  execLoopStmts :: [LoopStmt] -> InterpreterM StmtResult
  execLoopStmts [] = retenv
  execLoopStmts (ls:lstmts) = do
    res <- execLoopStmt ls
    case res of
      ReturnEnv env -> execLoopStmts lstmts
      _             -> return res

  execLoopStmt :: LoopStmt -> InterpreterM StmtResult
  execLoopStmt (SBreak pos) = return Break
  execLoopStmt (SCont pos) = return Cont
  execLoopStmt (LStmt pos stmt) = do
    res <- execStmt stmt
    return res
  execElif :: ElifStmt -> InterpreterM StmtResult
  execElif (SElifElse pos expr ifStmt (SElse pos2 elseStmt)) =
    execStmt (SIfElse pos expr ifStmt (SElse pos2 elseStmt))
  execElif (SElifElif pos expr ifStmt elif) = do
    val <- evalExpr expr
    if boolyVal val
      then execStmt ifStmt
      else execElif elif

  printExpr :: Expr -> InterpreterM ()
  printExpr e = do
    val <- evalExpr e
    liftIO $ putStr (show val)

  varsDecl' :: a -> [Ident] -> Value -> InterpreterM StmtResult
  varsDecl' pos [] val = retenv
  varsDecl' pos (id:idents) val = do
    env <- varDecl pos id val
    res <- local (const env) (varsDecl' pos idents val)
    return res

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
  evalExpr (ECmp pos e1 opCmp e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    let im1 = intyVal v1; im2 = intyVal v2; op = getHsCmpOp opCmp in
      case (v1, v2) of
        (VString s1, VString s2) -> return (VBool $ op s1 s2)
        _ -> case (im1, im2) of
          (Just i1, Just i2) -> return (VBool $ op i1 i2)
          _ -> throwError (showUnsupportedCmpOperandError opCmp v1 v2)
  evalExpr (EAriS pos e1 opAriS e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    let im1 = intyVal v1; im2 = intyVal v2; op = getHsAriSOp opAriS in
      case (v1, v2, opAriS) of
        (VString s1, VString s2, Pls _) -> return (VString $ s1 ++ s2) -- concat strings
        _ -> case (im1, im2) of
          (Just i1, Just i2) -> return (VInt $ op i1 i2)
          _ -> throwError (showUnsupportedAriSOperandError opAriS v1 v2)
  evalExpr (EAriUns pos e1 opAriUns e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    let im1 = intyVal v1; im2 = intyVal v2; op = getHsAriUnsOp opAriUns in
      case (im1, im2) of
        (Just i, Just 0) -> throwError showZeroDivError
        (Just i1, Just i2) -> return (VInt $ op i1 i2)
        _ -> throwError (showUnsupportedAriUnsOperandError opAriUns v1 v2)
  {-
  TODO
  | ECall a Ident [Expr' a]
  | ETern a (Expr' a) (Expr' a) (Expr' a)
  -}
  evalExpr _ = return VNull





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

  retenv :: InterpreterM StmtResult
  retenv = do
    env <- ask
    return (ReturnEnv env)
