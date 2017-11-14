module ProC.Data.HierarchicalMap
  ( HierarchicalMap
  , empty
  , push
  , pop
  , memberThisLevel
  , insert
  , update
  , lookup
  ) where

import           Prelude                hiding (lookup)

import           Control.Concurrent.STM

import qualified Data.Map               as M

data HierarchicalMap k v = HierarchicalMap
  { parent  :: Maybe (HierarchicalMap k v)
  , entries :: TVar (M.Map k v)
  }

empty :: STM (HierarchicalMap k v)
empty = HierarchicalMap Nothing <$> newTVar M.empty

push :: HierarchicalMap k v -> STM (HierarchicalMap k v)
push m = HierarchicalMap (Just m) <$> newTVar M.empty

pop :: Monad m => HierarchicalMap k v -> m (HierarchicalMap k v)
pop m = maybe (fail "Popped topmost level") return $ parent m

memberThisLevel :: Ord k => k -> HierarchicalMap k v -> STM Bool
memberThisLevel k m = M.member k <$> readTVar (entries m)

insert :: Ord k => k -> v -> HierarchicalMap k v -> STM ()
insert k v m = modifyTVar (entries m) (M.insert k v)

lookup :: Ord k => k -> HierarchicalMap k v -> STM (Maybe v)
lookup k m = maybe lookupInParent (return . Just) =<< lookupThisLevel
  where
    lookupThisLevel = M.lookup k <$> readTVar (entries m)
    lookupInParent = maybe (return Nothing) (lookup k) (parent m)

update :: (Show k, Ord k) => k -> v -> HierarchicalMap k v -> STM ()
update k v m = do
  isThisLevel <- memberThisLevel k m
  if isThisLevel
    then insert k v m
    else maybe (fail $ "Does not exist: " ++ show k) (update k v) (parent m)
