{-# LANGUAGE NoMonomorphismRestriction #-}

module Types where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.IO.Class
import qualified Data.Map.Lazy as M

import AbsSmolSnekGrammar

type InterpreterM a = ReaderT Env (ErrorT String (StateT Store IO)) a
type Loc = Int
type Env = M.Map Ident Loc
type Store = M.Map Loc Value
data Value = VInt Integer | VBool Bool | VString String | None | VFunc Env [Arg] Block | Mode String
  deriving (Eq, Ord)
data StmtResult = ReturnVal Value | ReturnEnv Env | Break | Cont
  deriving (Show)

emptyEnv = M.empty
emptyStore = M.empty

instance Show Value where
    show (VInt val)             = show val
    show (VBool b)              = show b
    show (VString str)          = str
    show None = ""
    show (VFunc env args block) = "function" ++ show args
    show (Mode str) = case str of
                        "/interactive" -> "<stdin>"
                        _              -> str

showValueType :: Value -> String
showValueType (VInt _) = "int"
showValueType (VBool _) = "bool"
showValueType (VString _) = "str"
showValueType (VFunc _ _ _) = "function"
showValueType (Mode _)      = "mode"
showValueType None = "NoneType"

getHsCmpOp :: Ord a => OpCmp -> a -> a -> Bool
getHsCmpOp op = case op of (Lt _)   -> (<)
                           (Gt _)  -> (>)
                           (Lte _) -> (<=)
                           (Gte _) -> (>=)
                           (Eq _)  -> (==)
                           (Ne _)  -> (/=)
getHsAriUnsOp :: Integral a => OpAriUns -> a -> a -> a
getHsAriUnsOp op = case op of (Mod _) -> (mod)
                              (Div _) -> (div)
getHsAriSOp :: Num a => OpAriS -> a -> a -> a
getHsAriSOp op = case op of (Mul _) -> (*)
                            (Add _) -> (+)
                            (Sub _) -> (-)
getHsIOp :: (Integral a, Num a) => IOp -> a -> a -> a
getHsIOp op = case op of IAdd _ -> (+)
                         ISub _ -> (-)
                         IMul _ -> (*)
                         IDiv _ -> (div)
                         IMod _ -> (mod)

{-
showOps :: Ops' a -> String
showOps op = case op of (Lt _) -> "<"
                        (Gt _)  -> ">"
                        (Lte _) -> "<="
                        (Gte _) -> ">="
                        (Eq _)  -> "=="
                        (Ne _)  -> "!="
                        (Mul _) -> "*"
                        (Pls _) -> "+"
                        (Mns _) -> "-"
                        (Mod _) -> "%"
                        (Div _) -> "/"
-}

-- TODO change to instance Show OpCmp were ...
showCmpOp :: OpCmp -> String
showCmpOp op = case op of (Lt _) -> "<"
                          (Gt _)  -> ">"
                          (Lte _) -> "<="
                          (Gte _) -> ">="
                          (Eq _)  -> "=="
                          (Ne _)  -> "!="
showAriSOp :: OpAriS -> String
showAriSOp op = case op of (Mul _) -> "*"
                           (Add _) -> "+"
                           (Sub _) -> "-"
showAriUnsOp :: OpAriUns -> String
showAriUnsOp op = case op of (Mod _) -> "%"
                             (Div _) -> "/"

boolyVal :: Value -> Bool
boolyVal val = case val of
                VBool False -> False -- falsy
                VInt 0 -> False
                VString "" -> False
                None -> False
                _ -> True            -- truthy

intyVal :: Value -> Maybe Integer
intyVal val = case val of
    VInt i -> Just i
    VBool True -> Just 1
    VBool False -> Just 0
    _ -> Nothing

getIdentsFromArgs :: [Arg] -> [Ident]
getIdentsFromArgs [] = []
getIdentsFromArgs ((Arg pos ident):args) = ident:(getIdentsFromArgs args)
