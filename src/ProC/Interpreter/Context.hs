module ProC.Interpreter.Context where

import ProC.Language

import Control.Monad.State

import qualified Data.Map as M

data Context = Context
  { variables :: M.Map Identifier Integer
  }

empty :: Context
empty = Context { variables = M.empty }

getVar :: Identifier -> Context -> Integer
getVar n c = case M.lookup n (variables c) of
  Just v  -> v
  Nothing -> error $ "Unknown: " ++ show n
  -- TODO: We should use Maybe here
  --       and have ContextM with an ErrorT (or similar) in the stack


setVar :: Identifier -> Integer -> Context -> Context
setVar n v c = c { variables = M.insert n v (variables c) }

type ContextM = StateT Context IO

getVarM :: Identifier -> ContextM Integer
getVarM n = get >>= return . getVar n

setVarM :: Identifier -> Integer -> ContextM ()
setVarM n v = modify (setVar n v)

evalContextM :: ContextM a -> IO a
evalContextM f = evalStateT f empty



