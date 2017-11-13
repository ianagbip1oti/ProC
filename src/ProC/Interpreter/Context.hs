{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module ProC.Interpreter.Context
  ( ContextM
  , evalContextM
  , setVarM
  , getVarM
  , enterBlockM
  , exitBlockM
  ) where

import           ProC.Language

import           Control.Monad.State
import           Data.Dynamic
import qualified Data.Map            as M

data Context = Context
  { parent    :: Maybe Context
  , variables :: M.Map Identifier Dynamic
  }

empty :: Context
empty = Context {parent = Nothing, variables = M.empty}

enterBlock :: Context -> Context
enterBlock c = Context {parent = Just c, variables = M.empty}

exitBlock :: (Monad m) => Context -> m Context
exitBlock c =
  case parent c of
    Nothing -> fail "Exited topmost Context"
    Just p  -> return p

getVar :: (Monad m, Typeable a) => Identifier -> Context -> m a
getVar idn c =
  case M.lookup idn (variables c) of
    Nothing -> maybe (fail $ "Unknown: " ++ show idn) (getVar idn) (parent c)
    Just v  -> maybe (fail $ "Bad Type: " ++ show idn) return (fromDynamic v)

setVar :: Typeable a => Identifier -> a -> Context -> Context
setVar idn v c = c {variables = M.insert idn (toDyn v) (variables c)}

type ContextM = StateT Context IO

getVarM :: Typeable a => Identifier -> ContextM a
getVarM p = get >>= getVar p

setVarM :: Typeable a => Identifier -> a -> ContextM ()
setVarM n v = modify (setVar n v)

enterBlockM :: ContextM ()
enterBlockM = modify enterBlock

exitBlockM :: ContextM ()
exitBlockM = get >>= exitBlock >>= put

evalContextM :: ContextM a -> IO a
evalContextM f = evalStateT f empty
