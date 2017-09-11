{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module ProC.Interpreter.Context
  ( ContextM
  , evalContextM
  , setVarM
  , getVarM
  ) where

import           ProC.Language

import           Control.Monad.State
import qualified Data.Map            as M

data PValue
  = PIntValue Integer
  | PStrValue String
  deriving (Show)

class PTypeMapping a where
  type HType a :: *
  wrapValue :: PVar a -> HType a -> PValue
  unwrapValue :: (Monad m) => PVar a -> PValue -> m (HType a)

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

newtype Context = Context
  { variables :: M.Map Identifier PValue
  }

empty :: Context
empty = Context {variables = M.empty}

getVar :: (Monad m, PTypeMapping p) => PVar p -> Context -> m (HType p)
getVar p c =
  case M.lookup (getIdentifier p) (variables c) of
    Nothing -> fail $ "Unknown: " ++ show p
    Just v  -> unwrapValue p v

setVar :: (PTypeMapping p) => PVar p -> HType p -> Context -> Context
setVar n v c =
  c {variables = M.insert (getIdentifier n) (wrapValue n v) (variables c)}

type ContextM = StateT Context IO

getVarM :: (PTypeMapping p) => PVar p -> ContextM (HType p)
getVarM p = get >>= getVar p

setVarM :: (PTypeMapping p) => PVar p -> HType p -> ContextM ()
setVarM n v = modify (setVar n v)

evalContextM :: ContextM a -> IO a
evalContextM f = evalStateT f empty
