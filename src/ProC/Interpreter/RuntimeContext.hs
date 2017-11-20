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

import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.Dynamic

type RuntimeContext = HM.HMap Identifier Dynamic

empty :: MonadIO m => m RuntimeContext
empty = liftIO (atomically HM.empty)

getVar ::
     (Monad m, MonadIO m, Typeable a) => Identifier -> RuntimeContext -> m a
getVar idn c = maybe unknown from =<< (liftIO . atomically . HM.lookup idn) c
  where
    from v = maybe (fail $ "Bad Type: " ++ show idn) return $ fromDynamic v
    unknown = fail $ "Unknown: " ++ show idn

declareVar ::
     (MonadIO m, Typeable a) => Identifier -> a -> RuntimeContext -> m ()
declareVar idn v = liftIO . atomically . HM.insert idn (toDyn v)

setVar :: (MonadIO m, Typeable a) => Identifier -> a -> RuntimeContext -> m ()
setVar idn v = liftIO . atomically . HM.update idn (toDyn v)

type RuntimeContextM = StateT RuntimeContext IO

getVarM :: Typeable a => Identifier -> RuntimeContextM a
getVarM p = get >>= getVar p

declareVarM :: Typeable a => Identifier -> a -> RuntimeContextM ()
declareVarM idn v = get >>= declareVar idn v

setVarM :: Typeable a => Identifier -> a -> RuntimeContextM ()
setVarM n v = get >>= setVar n v

enterBlockM :: RuntimeContextM ()
enterBlockM = get >>= liftIO . atomically . HM.push >>= put

exitBlockM :: RuntimeContextM ()
exitBlockM = get >>= liftIO . atomically . HM.pop >>= put

evalContextM :: RuntimeContextM a -> IO a
evalContextM f = evalStateT f =<< empty
