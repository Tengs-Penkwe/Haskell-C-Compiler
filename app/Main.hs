module Main where 

import Parser
import Codegen
import Emit

import Control.Monad.Trans
import Data.String

import System.IO (readFile, writeFile)
import System.Environment (getArgs)
import System.Console.Haskeline
import System.Console.GetOpt

import LLVM.AST 
import qualified LLVM.AST as AST

initModule :: AST.Module
initModule = defaultModule { moduleName = fromString "Haskell C Compiler" }

process :: String -> IO String
process content = do
  let res = parseProgram content
  case res of 
    Left err -> print err >> return ""
    Right ex -> do
      mapM_ print ex
      ir <- codegen initModule ex
      return  ir

repl :: IO()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    ("-i":infile:"-o":outfile:[]) -> do
      content <- readFile infile
      ir <- process content
      writeFile outfile ir
    _ -> error "Usage: program -i <input_file> -o <output_file>"
