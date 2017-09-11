{-# LANGUAGE DataKinds #-}

module ProC.Interpreter.Context
  ( ContextM
  , evalContextM
  , setVarM
  , getVarM
  ) where

import           ProC.Language

import           Control.Monad.State

import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)

newtype Context = Context
  { variables :: M.Map (PVar 'PInt) Integer
  }

empty :: Context
empty = Context {variables = M.empty}

getVar :: PVar 'PInt -> Context -> Integer
getVar n c =
  fromMaybe (error $ "Unknown: " ++ show n) $ M.lookup n (variables c)
  -- TODO: We should use Maybe here
  --       and have ContextM with an ErrorT (or similar) in the stack

setVar :: PVar 'PInt -> Integer -> Context -> Context
setVar n v c = c {variables = M.insert n v (variables c)}

type ContextM = StateT Context IO

getVarM :: PVar 'PInt -> ContextM Integer
getVarM n = getVar n <$> get

setVarM :: PVar 'PInt -> Integer -> ContextM ()
setVarM n v = modify (setVar n v)

evalContextM :: ContextM a -> IO a
evalContextM f = evalStateT f empty
