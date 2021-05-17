module Main where

import Prelude
  ( ($)
  , Either(..)
  , Int, (>)
  , String, (++), unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )
import System.IO ( stdin, stderr, hGetContents, hPutStrLn )

import qualified Control.Exception as E

import AbsSmolSnekGrammar   ()
import LexSmolSnekGrammar   ( Token )
import ParSmolSnekGrammar   ( pProgram, myLexer )
import PrintSmolSnekGrammar ( Print, printTree )
import SkelSmolSnekGrammar  ()
import Interpreter
import Types
import ErrM

type ParseFun a = [Token] -> Err a
type Verbosity  = Int

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> usage -- TODO interactive mode coś a\la to: getContents >>= run 2 pProgram
    (filename:_)     -> do
      programStr <- readFile filename
      case (pProgram (myLexer programStr)) of
        Left err -> do
              putStrLn err
              exitFailure
        Right tree -> do
              --showTree 2 tree
              (val, store) <- runInterpreterM emptyEnv emptyStore (runProgram tree)
              case val of
                Left e  -> hPutStrLn stderr e
                Right code -> hPutStrLn stderr $ "Exit code: " ++ show code
              exitSuccess
              
    --[]         -> getContents >>= run 2 pProgram
    --"-s":fs    -> mapM_ (runFile 0 pProgram) fs
    --fs         -> mapM_ (runFile 2 pProgram) fs

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      putStrV v $ show ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      showTree v tree
      exitSuccess
  where
  ts = myLexer s

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    ]
  exitFailure
