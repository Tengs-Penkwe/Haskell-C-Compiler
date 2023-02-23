module Main where 

import Parser

import Control.Monad.Trans

import System.IO                --readFile
import System.Environment       -- getArgs
import System.Console.Haskeline

process :: String -> IO ()
process content = do
  let res = parseProgram content
  case res of 
    Left err -> print err
    Right ex -> mapM_ print ex

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
    [file]  -> readFile file >>= process
