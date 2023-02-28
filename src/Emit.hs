module Emit where

import qualified Syntax as S

import LLVM.Context(withContext)

import qualified LLVM.AST as AST

import Control.Monad.Except
{-- ==================================================
 -      Produce Code
 - ================================================== --}
liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

-- codegen :: AST.Module -> [S.Unit] -> IO AST.Module
-- codegen modul units 
--   = withContext $ \context -> 
