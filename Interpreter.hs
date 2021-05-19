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
    insertValue 0 (Mode filename)
    res <- execStmts stmts
    case res of
      ReturnVal val -> liftIO $ putStrLn $ "retval possibly error" ++ show val -- TODO to tez chyba error
      ReturnEnv env -> liftIO $ putStrLn $ "env :" ++ show env
      bc            -> liftIO $ putStrLn $ showSyntaxError bc
    store <- get
    liftIO $ putStrLn $ "store: " ++ show store
    return 0

  execBlock :: Block -> InterpreterM StmtResult -- TODO zmiany w blocku powinny sie cofac po wyjsciu z niego
  execBlock (Block pos stmts) = execStmts stmts

  execStmts :: [Stmt] -> InterpreterM StmtResult
  execStmts [] = retenv
  execStmts (s:ss) = do
    res <- execStmt s
    case res of
      (ReturnEnv env) -> do
        {-
        store <- get
        printDebug ("execed stmt: " ++ show s) env store
        -}
        local (const env) (execStmts ss)
      _ -> return res -- return e/continue/break all stop the execution of other stmts
      -- correctness of them will be determined by the caller

  execStmt :: Stmt -> InterpreterM StmtResult

  ---- VAR ASSIGNMENT
  execStmt (VarDef pos ident expr) = do
    val <- evalExpr expr
    mloc <- getVarLoc ident              -- this breaks shadowing in func in func
    loc <- case mloc of
            Nothing  -> gets (getNextLoc)
            Just loc -> return loc
    --loc <- gets (getNextLoc)           -- this breaks while
    if loc > 0
      then do
        env <- asks (M.insert ident loc)
        insertValue loc val
        return (ReturnEnv env)
      else
        throwError $ showSyntaxErrorReadOnly ident

  ---- DEF FUN
  execStmt (FunDef pos ident args block) = do
    loc <- gets (getNextLoc)
    env <- asks (M.insert ident loc)
    insertValue loc (VFunc env args block)
    return (ReturnEnv env)

  ---- PRINT
  execStmt (SPrint pos []) = liftIO (putStrLn "") >> retenv
  execStmt (SPrint pos (e:exprs)) = printExpr e >> liftIO (putStr " ") >> execStmt (SPrint pos exprs)

  ---- IF ELSE ELIF
  execStmt (SIf pos cond block) = do
    val <- evalExpr cond
    if boolyVal val
      then execBlock block
      else retenv
  execStmt (SIfElse pos cond ifBlock (SElse pos2 elseBlock)) = do
    val <- evalExpr cond
    if boolyVal val
      then execBlock ifBlock
      else execBlock elseBlock
  execStmt (SIfElif pos cond ifBlock elif) = do
    val <- evalExpr cond
    if boolyVal val
      then execBlock ifBlock
      else execElif elif

  ---- BREAK CONTINUE RETURN
  execStmt (SBreak pos) = return Break
  execStmt (SCont pos) = return Cont
  execStmt (SReturn pos expr) = do
    val <- evalExpr expr
    return (ReturnVal val)


  ---- DANGLING EXPR
  execStmt (Expr pos expr) = do
    m <- getVarValue 0
    case m of
      Mode "/interactive" -> printExpr expr >> retenv
      _                   -> do
        {-
        env <- ask
        store <- get
        printDebug "execStmt Expr before eval" env store -}
        env <- ask
        evalExpr expr
        return (ReturnEnv env)
        --evalExpr expr
        --e <- ask
        --s <- get
        --printDebug "execStmt Expr after eval" env s
        --retenv

  -- IN PLACE OPERATION
  execStmt (SIOp pos ident iop expr) = do
    curr <- getVar ident
    val <- evalExpr expr
    newVal <- (let imCurr = intyVal curr; imVal = intyVal val in
                case iop of
                  (IAdd _) -> do
                    case (curr, val) of
                      -- concat strings
                      (VString currStr, VString valStr) -> return (VString $ currStr ++ valStr)
                      -- else add if possible
                      _ -> do
                        case (imCurr, imVal) of
                          (Just iCurr, Just iVal) -> return (VInt $ iCurr + iVal)
                          _ -> throwError (showUnsupportedOperand "+=" curr val)
                  (ISub _) -> do
                    case (imCurr, imVal) of
                      (Just iCurr, Just iVal) -> return (VInt $ iCurr - iVal)
                      _ -> throwError (showUnsupportedOperand "-=" curr val)
                  (IMul _) -> do
                    case (imCurr, imVal) of
                      (Just iCurr, Just iVal) -> return (VInt $ iCurr * iVal)
                      _ -> throwError (showUnsupportedOperand "*=" curr val)
                  -- unsafe divison by 0
                  (IDiv _) -> do
                    case (imCurr, imVal) of
                      (Just i, Just 0) -> throwError showZeroDivError
                      (Just iCurr, Just iVal) -> return (VInt $ div iCurr iVal)
                      _ -> throwError (showUnsupportedOperand "/=" curr val)
                  (IMod _) -> do
                    case (imCurr, imVal) of
                      (Just i, Just 0) -> throwError showZeroDivError
                      (Just iCurr, Just iVal) -> return (VInt $ mod iCurr iVal)
                      _ -> throwError (showUnsupportedOperand "%=" curr val)
                )
    setVar ident newVal
    retenv

  ---- WHILE LOOP
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


  ---- RANGE LOOP
  execStmt (SRange pos ident rangeExpr block) = do
    val <- evalExpr rangeExpr
    case intyVal val of
      Nothing -> throwError $ showIntyTypeError (showValueType val)
      Just range -> do
        env <- declareReadOnlyVar ident 0
        res <- local (const env) $ execRange pos ident range block
        {-
        e' <- ask
        s <- get
        printDebug "after execRange " e' s
        liftIO $ putStrLn $ show res
        -}
        retenv

  execRange :: BNFC'Position -> Ident -> Integer -> Block -> InterpreterM StmtResult
  execRange pos ident range block = do
    val <- getVar ident
    case val of
      (VInt idx) -> do
        if idx >= range
          then retenv
          else do
            res <- execBlock block
            case res of
              Break -> retenv
              ReturnVal val -> return res
              _ -> do
                env <- incrementReadOnlyVar ident
                local (const env) $ execRange pos ident range block
      _ -> throwError $ "range error hmmm"


  declareReadOnlyVar :: Ident -> Integer -> InterpreterM Env
  declareReadOnlyVar ident int = do
    loc <- gets (negate . getNextLoc)
    env <- asks (M.insert ident loc)
    insertValue loc (VInt int)
    return env

  incrementReadOnlyVar :: Ident -> InterpreterM Env
  incrementReadOnlyVar ident = do
    mloc <- getVarLoc ident
    case mloc of
      Nothing  -> throwError $ "unexpected error w incrementReadOnlyVar"
      Just loc -> do
        currVal <- getVar ident
        case currVal of
          (VInt i) -> do
            env <- asks (M.insert ident loc)
            insertValue loc (VInt (i+1))
            return env
          _ -> throwError $ "unexpected error odczytujac read only val w incrementReadOnlyVar"

{-
  ---- RANGE LOOP
  execStmt (SRange pos ident rangeExpr block) = do
    {- DEBUG -}
    env <- ask
    store <- get
    printDebug  "exec SRange" env store
    {- -}
    val <- evalExpr rangeExpr
    case intyVal val of
      Nothing -> throwError $ showIntyTypeError (showValueType val)
      Just toVal -> do
        env <- ask
        local (const env) (range' pos ident 0 toVal block)


  range' ::  BNFC'Position -> Ident -> Integer -> Integer -> Block -> InterpreterM StmtResult
  range' pos ident fromVal toVal block = do
    {- DEBUG -}
    env <- ask
    store <- get
    --printDebug  ("begin range' args: " ++ show fromVal ++ show toVal) env store
    {- -}
    env <- if fromVal == 0
            then declareReadOnlyVar ident 0
            else ask
    {- DEBUG -}
    store <- get
    --printDebug "after declaring read only " env store
    {- -}
    {- DEBUG
    env' <- ask
    store <- get
    printDebug "ask after declaring read only " env' store
     -}
    val <- local (const env) $ getVar ident
    --liftIO $ putStrLn $ "got read only var value and it is: " ++ show val
    case val of
      VInt i -> do
        if i < toVal
          then do
            res <- local (const env) $ execBlock block
            mloc <- local (const env) $ getVarLoc ident
            let incremented = i+1 in
              case mloc of
                Just loc -> do
                  liftIO $ putStrLn $ "putting " ++ show incremented ++ " at " ++ show loc
                  insertValue loc (VInt incremented)
                  case res of
                    ReturnEnv e -> local (const e) $ range' pos ident incremented toVal block
                    Cont          -> local (const env) $ range' pos ident incremented toVal block
                    Break         -> retenv
                    ReturnVal val -> return (ReturnVal val)
                Nothing -> throwError $ "coś takiego nie powinno sie zdarzyc nie można znaleźć zmiennej read only"
          else retenv
      _ -> throwError $ "no coś takiego nie powinno sie zdarzyc read only variable ma typ inny niz int"



  -}

  execElif :: ElifStmt -> InterpreterM StmtResult
  execElif (SElifElse pos cond ifBlock else') =
    execStmt (SIfElse pos cond ifBlock else')
  execElif (SElifElif pos cond ifBlock elif) = do
    val <- evalExpr cond
    if boolyVal val
      then execBlock ifBlock
      else execElif elif

  printExpr :: Expr -> InterpreterM ()
  printExpr e = do
    val <- evalExpr e
    liftIO $ putStr $ id $ (show val)
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


  evalExpr (ETern pos ifExpr cond elseExpr) = do
    val <- evalExpr cond
    if boolyVal val
      then evalExpr ifExpr
      else evalExpr elseExpr

  evalExpr (ECall pos ident exprs) = do
    argVals <- evalExprs exprs
    val <- getVar ident
    case val of
      (VFunc env args block) -> do
        let argIdents = (getIdentsFromArgs args); takes = length argIdents; given = length argVals in
          if  takes /= given
            then throwError $ showArgLenTypeError ident takes given
            else do


              s <- get
              printDebug ("env from " ++ show ident ++ " def ") env s

              env' <- local (const env) $ assignArgVals argIdents argVals

              s <- get
              printDebug ("env from " ++ show ident ++ " def after assigning ") env' s

              res <- local (const env') $ execBlock block

              s <- get
              printDebug ("env from " ++ show ident ++ "  def after block ") env' s
              e <- ask
              s <- get
              printDebug "ask after block " e s

              case res of
                ReturnEnv env -> return None
                ReturnVal val -> return val
                bc -> throwError $ showSyntaxError bc
      _ -> throwError $ showCalTypeError (showValueType val)

  evalExprs :: [Expr] -> InterpreterM [Value]
  evalExprs [] = return []
  evalExprs (e:es) = do
    v <- evalExpr e
    vs <- evalExprs es
    return (v:vs)

  assignArgVals :: [Ident] -> [Value] -> InterpreterM Env
  assignArgVals [] [] = ask
  assignArgVals (i:idents) (v:vals) = do
    mloc <- getVarLoc i
    loc <- gets (getNextLoc)
    env <- asks (M.insert i loc)
    insertValue loc v
    local (const env) $ assignArgVals idents vals
  assignArgVals _ _ = throwError $ "unexpected error in assignArgVals idents args differ in length"

  getVar :: Ident -> InterpreterM Value
  getVar ident = do
    mloc <- getVarLoc ident
    case mloc of
      Nothing    -> throwError $ showNameError ident
      (Just loc) -> getVarValue loc

  setVar :: Ident -> Value -> InterpreterM ()
  setVar ident val = do
    mloc <- getVarLoc ident
    case mloc of
      Nothing    -> throwError $ showNameError ident
      (Just loc) -> insertValue loc val

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

  printDebug context env store = do
    liftIO $ putStrLn $ context ++ "\nenv: " ++ show env ++ "\nstore: " ++ show store ++ "\n"
