module ProC.Interpreter.Context where

import Control.Monad.State

import qualified Data.Map as M

data Context = Context
  { variables :: M.Map String Integer
  }

empty :: Context
empty = Context { variables = M.empty }

getVar :: String -> Context -> Maybe Integer
getVar n c = M.lookup n (variables c)
  Just v  -> v
  -- TODO: We should use Maybe here
  --       and have ContextM with an ErrorT (or similar) in the stack
  Nothing -> error $ "Unknown var: " ++ n

setVar :: String -> Integer -> Context -> Context
setVar n v c = c { variables = M.insert n v (variables c) }

type ContextM = StateT Context IO

getVarM :: String -> ContextM Integer
getVarM n = get >>= return . getVar n

setVarM :: String -> Integer -> ContextM ()
setVarM n v = modify (setVar n v)

evalContextM :: ContextM a -> IO a
evalContextM f = evalStateT f empty



