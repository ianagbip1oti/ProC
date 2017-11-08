{-# LANGUAGE DataKinds    #-}
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
import qualified Data.Map            as M

data PValue
  = PBlnValue Bool
  | PIntValue Integer
  | PStrValue String
  deriving (Show)

class PTypeMapping a where
  type HType a :: *
  wrapValue :: PVar a -> HType a -> PValue
  unwrapValue :: (Monad m) => PVar a -> PValue -> m (HType a)

instance PTypeMapping 'PBln where
  type HType 'PBln = Bool
  wrapValue _ = PBlnValue
  unwrapValue _ (PBlnValue b) = return b
  unwrapValue _ v             = fail $ "Invalid value " ++ show v

instance PTypeMapping 'PInt where
  type HType 'PInt = Integer
  wrapValue _ = PIntValue
  unwrapValue _ (PIntValue i) = return i
  unwrapValue _ v             = fail $ "Invalid value " ++ show v

instance PTypeMapping 'PStr where
  type HType 'PStr = String
  wrapValue _ = PStrValue
  unwrapValue _ (PStrValue s) = return s
  unwrapValue _ v             = fail $ "Invalid value " ++ show v

data Context = Context
  { parent    :: Maybe Context
  , variables :: M.Map Identifier PValue
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

getVar :: (Monad m, PTypeMapping p) => PVar p -> Context -> m (HType p)
getVar p c =
  case M.lookup (getIdentifier p) (variables c) of
    Nothing -> maybe (fail $ "Unknown: " ++ show p) (getVar p) (parent c)
    Just v  -> unwrapValue p v

setVar :: (PTypeMapping p) => PVar p -> HType p -> Context -> Context
setVar n v c =
  c {variables = M.insert (getIdentifier n) (wrapValue n v) (variables c)}

type ContextM = StateT Context IO

getVarM :: (PTypeMapping p) => PVar p -> ContextM (HType p)
getVarM p = get >>= getVar p

setVarM :: (PTypeMapping p) => PVar p -> HType p -> ContextM ()
setVarM n v = modify (setVar n v)

enterBlockM :: ContextM ()
enterBlockM = modify enterBlock

exitBlockM :: ContextM ()
exitBlockM = get >>= exitBlock >>= put

evalContextM :: ContextM a -> IO a
evalContextM f = evalStateT f empty
