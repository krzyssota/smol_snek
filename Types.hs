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
data Value = VInt Integer | VBool Bool | VString String | VFunc Env [Arg] Block | VNull
    deriving (Eq, Ord)

emptyEnv = M.empty
emptyStore = M.empty


instance Show Value where
    show (VInt val)             = show val
    show (VBool b)              = show b
    show (VString str)          = show str
    show (VFunc env args block) = "Func"
    show (VNull)                = "Null"
