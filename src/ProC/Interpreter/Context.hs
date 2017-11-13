{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module ProC.Interpreter.Context
  ( ContextM
  , declareVarM
  , evalContextM
  , setVarM
  , getVarM
  , enterBlockM
  , exitBlockM
  ) where

import           ProC.Language

import           Control.Lens        (Traversal', at, makeLenses, (.~), (?~),
                                      (^.), (^?), _Just)
import           Control.Monad.State
import           Data.Dynamic
import qualified Data.Map            as M

data Context = Context
  { _parent    :: Maybe Context
  , _variables :: M.Map Identifier Dynamic
  }

makeLenses ''Context

empty :: Context
empty = Context {_parent = Nothing, _variables = M.empty}

enterBlock :: Context -> Context
enterBlock c = Context {_parent = Just c, _variables = M.empty}

exitBlock :: (Monad m) => Context -> m Context
exitBlock c = maybe (fail "Exited topmost Context") return $ c ^. parent

varThisContext :: Identifier -> Traversal' Context Dynamic
varThisContext idn = variables . at idn . _Just

contextWith :: Identifier -> Context -> Traversal' Context Context
contextWith idn ctx =
  case ctx ^? varThisContext idn of
    Just _ -> id
    Nothing ->
      case ctx ^. parent of
        Just p  -> parent . _Just . contextWith idn p
        Nothing -> id

variable :: Identifier -> Context -> Traversal' Context Dynamic
variable idn ctx = contextWith idn ctx . varThisContext idn

getVar :: (Monad m, Typeable a) => Identifier -> Context -> m a
getVar idn c = maybe (fail $ "Unknown: " ++ show idn) from $ c ^? variable idn c
  where
    from v = maybe (fail $ "Bad Type: " ++ show idn) return $ fromDynamic v

declareVar :: Typeable a => Identifier -> a -> Context -> Context
declareVar idn v = variables . at idn ?~ toDyn v

setVar :: Typeable a => Identifier -> a -> Context -> Context
setVar idn v c = variable idn c .~ toDyn v $ c

type ContextM = StateT Context IO

getVarM :: Typeable a => Identifier -> ContextM a
getVarM p = get >>= getVar p

declareVarM :: Typeable a => Identifier -> a -> ContextM ()
declareVarM n v = modify $ declareVar n v

setVarM :: Typeable a => Identifier -> a -> ContextM ()
setVarM n v = modify $ setVar n v

enterBlockM :: ContextM ()
enterBlockM = modify enterBlock

exitBlockM :: ContextM ()
exitBlockM = get >>= exitBlock >>= put

evalContextM :: ContextM a -> IO a
evalContextM f = evalStateT f empty
