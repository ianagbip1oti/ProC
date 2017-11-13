module ProC.Interpreter.RuntimeContext
  ( RuntimeContextM
  , declareVarM
  , evalContextM
  , setVarM
  , getVarM
  , enterBlockM
  , exitBlockM
  ) where

import qualified ProC.Data.HierarchicalMap as HM
import           ProC.Language

import           Control.Monad.State
import           Data.Dynamic

type RuntimeContext = HM.HierarchicalMap Identifier Dynamic

empty :: RuntimeContext
empty = HM.empty

getVar :: (Monad m, Typeable a) => Identifier -> RuntimeContext -> m a
getVar idn c = maybe (fail $ "Unknown: " ++ show idn) from $ HM.lookup idn c
  where
    from v = maybe (fail $ "Bad Type: " ++ show idn) return $ fromDynamic v

declareVar :: Typeable a => Identifier -> a -> RuntimeContext -> RuntimeContext
declareVar idn v = HM.insert idn $ toDyn v

setVar :: Typeable a => Identifier -> a -> RuntimeContext -> RuntimeContext
setVar idn v = HM.update idn $ toDyn v

type RuntimeContextM = StateT RuntimeContext IO

getVarM :: Typeable a => Identifier -> RuntimeContextM a
getVarM p = get >>= getVar p

declareVarM :: Typeable a => Identifier -> a -> RuntimeContextM ()
declareVarM n v = modify $ declareVar n v

setVarM :: Typeable a => Identifier -> a -> RuntimeContextM ()
setVarM n v = modify $ setVar n v

enterBlockM :: RuntimeContextM ()
enterBlockM = modify HM.push

exitBlockM :: RuntimeContextM ()
exitBlockM = get >>= HM.pop >>= put

evalContextM :: RuntimeContextM a -> IO a
evalContextM f = evalStateT f empty
