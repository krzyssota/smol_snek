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

  runProgram :: Program -> String -> InterpreterM Integer
  runProgram (Program pos stmts) filename = do -- TODO store filename smh and use it in errors
    res <- execStmts stmts
    case res of
      ReturnVal val -> liftIO $ putStrLn $ "retval possibly error" ++ show val -- TODO to tez chyba error
      ReturnEnv env -> liftIO $ putStrLn $ "retenv " ++ show env
      bc            -> liftIO $ putStrLn $ showSyntaxError bc
    store <- get
    liftIO $ putStrLn $ show store
    return 0

  execBlock :: Block -> InterpreterM StmtResult -- TODO może zmergować execBlock/Stmts
  execBlock (Block pos stmts) = execStmts stmts

  execStmts :: [Stmt] -> InterpreterM StmtResult
  execStmts [] = retenv
  execStmts (s:ss) = do
    res <- execStmt s
    case res of
      (ReturnEnv env) -> do
        local (const env) (execStmts ss)
      _ -> return res -- return e/continue/break all stop the execution of other stmts
      -- correctness of them will be determined by the caller

  execStmt :: Stmt -> InterpreterM StmtResult
  execStmt (VarDef pos ident expr) = do
    val <- evalExpr expr
    mloc <- getVarLoc ident
    loc <- case mloc of
            Nothing  -> gets (getNextLoc)
            Just loc -> return loc
    env <- asks (M.insert ident loc)
    insertValue loc val
    return (ReturnEnv env)
  execStmt (FunDef pos ident args block) = do
    loc <- gets (getNextLoc)
    env <- asks (M.insert ident loc)
    insertValue loc (VFunc env args block)
    return (ReturnEnv env)
  execStmt (SPrint pos []) = liftIO (putStrLn "") >> retenv
  execStmt (SPrint pos (e:exprs)) = printExpr e >> liftIO (putStr " ") >> execStmt (SPrint pos exprs)
  execStmt (SIf pos expr block) = do
    val <- evalExpr expr
    if boolyVal val
      then execBlock block
      else retenv
  execStmt (SIfElse pos expr ifBlock (SElse pos2 elseBlock)) = do
    val <- evalExpr expr
    if boolyVal val
      then execBlock ifBlock
      else execBlock elseBlock
  execStmt (SIfElif pos expr ifBlock elif) = do
    val <- evalExpr expr
    if boolyVal val
      then execBlock ifBlock
      else execElif elif
  execStmt (SWhile pos expr block) = do
    val <- evalExpr expr
    if boolyVal val
      then do
        res <- execBlock block
        case res of
          ReturnEnv env -> execStmt (SWhile pos expr block)
          Cont          -> execStmt (SWhile pos expr block)
          Break         -> retenv
          ReturnVal val -> return (ReturnVal val)
      else retenv
  execStmt (SBreak pos) = return Break
  execStmt (SCont pos) = return Cont
  -- TODO usunac zeby wiedziec ze wszyskto jest pokryte
  --execStmt _ = do
  --  env <- ask
  --  return (ReturnEnv env)

  execElif :: ElifStmt -> InterpreterM StmtResult
  execElif (SElifElse pos expr ifBlock else') =
    execStmt (SIfElse pos expr ifBlock else')
  execElif (SElifElif pos expr ifBlock elif) = do
    val <- evalExpr expr
    if boolyVal val
      then execBlock ifBlock
      else execElif elif

  printExpr :: Expr -> InterpreterM ()
  printExpr e = do
    val <- evalExpr e
    liftIO $ putStr (show val)
{-
  varsDecl' :: a -> [Ident] -> Value -> InterpreterM StmtResult -- TODO wywalić
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
-}
  evalExpr :: Expr -> InterpreterM Value
  evalExpr (EVar pos ident) = getVar ident
  evalExpr (EStrLit pos s) = return (VString s)
  evalExpr (EIntLit pos i) = return (VInt i)
  evalExpr (ETrueLit pos) = return (VBool True)
  evalExpr (EFalseLit pos) = return (VBool False)
  evalExpr (ENeg pos unOp expr) = do
    val <- evalExpr expr
    case unOp of
      -- -
      (OpAriNeg _) -> case intyVal val of
                        (Just i) -> return (VInt (negate i))
                        Nothing  -> throwError $ showUnOpTypeError unOp (showValueType val)
      -- NOT
      (OpLogNeg _) -> return (VBool (not $ boolyVal val))
  evalExpr (ECompos pos e1 binOp e2) = do
    case binOp of
      -- / %
      (OpAriUns pos op) -> do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        let im1 = intyVal v1; im2 = intyVal v2; hsOp = getHsAriUnsOp op in
          case (im1, im2) of
            (Just i, Just 0) -> throwError showZeroDivError
            (Just i1, Just i2) -> return (VInt $ hsOp i1 i2)
            _ -> throwError (showUnsupportedAriUnsOperandError op v1 v2)
      -- + - *
      (OpAriS pos op) -> do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        let im1 = intyVal v1; im2 = intyVal v2; hsOp = getHsAriSOp op in
          case (v1, v2, op) of
            (VString s1, VString s2, Add _) -> return (VString $ s1 ++ s2) -- concat strings
            _ -> case (im1, im2) of
                  (Just i1, Just i2) -> return (VInt $ hsOp i1 i2)
                  _ -> throwError (showUnsupportedAriSOperandError op v1 v2)
      -- AND OR
      (OpLog pos op) -> do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        let b1 = boolyVal v1; b2 = boolyVal v2 in
          case op of
            (And pos) -> return (VBool $ b1 && b2)
            (Or pos)  -> return (VBool $ b1 || b2)
      -- < <= > >= == !=
      (OpCmp pos op) -> do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        let im1 = intyVal v1; im2 = intyVal v2; opHs = getHsCmpOp op in
          case (v1, v2) of
            (VString s1, VString s2) -> return (VBool $ opHs s1 s2)
            _ -> case (im1, im2) of
                  (Just i1, Just i2) -> return (VBool $ opHs i1 i2)
                  _ -> throwError (showUnsupportedCmpOperandError op v1 v2)


  {-
  TODO
  | ECall a Ident [Expr' a]
  | ETern a (Expr' a) (Expr' a) (Expr' a)
  -}
  evalExpr _ = undefined


  getVar :: Ident -> InterpreterM Value
  getVar ident = do
    mloc <- getVarLoc ident
    case mloc of
      Nothing    -> throwError $ showNameError ident
      (Just loc) -> getVarValue loc

  getVarLoc :: Ident -> InterpreterM (Maybe Loc)
  getVarLoc ident = asks (M.lookup ident)

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

  printEnvStore = do
    env <- ask
    store <- get
    liftIO $ putStrLn $ show store
