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

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = fromString label }

initModule :: AST.Module
initModule = emptyModule "Haskell C Compiler"

process :: String -> IO String
process content = do
  let res = parseProgram content
  case res of 
    Left err -> print err >> return ""
    Right ex -> do
      -- let ir = genProgram ex  
      mapM_ print ex
      return  " " --ir

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
    []      -> repl
    [file]  -> do  --readFile file >>= process
      content <- readFile file
      ir      <- process content
      writeFile "output.ll" ir
