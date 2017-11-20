{-# LANGUAGE LambdaCase #-}

module ProC.Data.HierarchicalMap
  ( HMap
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
import           Control.Monad

import           Data.Fix

import qualified Data.Map               as M

data HMapF k v a
  = Top
  | HM a
       (TVar (M.Map k v))

type HMap k v = Fix (HMapF k v)

instance Functor (HMapF k v) where
  fmap _ Top      = Top
  fmap f (HM a e) = HM (f a) e

instance Foldable (HMapF k v) where
  foldMap _ Top      = mempty
  foldMap f (HM a _) = f a

instance Traversable (HMapF k v) where
  traverse _ Top      = pure Top
  traverse f (HM a e) = HM <$> f a <*> pure e

parent :: Monad m => HMap k v -> m (HMap k v)
parent m =
  case unFix m of
    Top    -> fail "No parent"
    HM p _ -> return p

entries :: HMap k v -> STM (M.Map k v)
entries m =
  case unFix m of
    Top    -> return M.empty
    HM _ e -> readTVar e

modifyEntries :: HMap k v -> (M.Map k v -> M.Map k v) -> STM ()
modifyEntries m f =
  case unFix m of
    Top    -> fail "Can not modify entries of TOP"
    HM _ e -> modifyTVar e f

empty :: STM (HMap k v)
empty = Fix . HM (Fix Top) <$> newTVar M.empty

push :: HMap k v -> STM (HMap k v)
push m = Fix . HM m <$> newTVar M.empty

pop :: Monad m => HMap k v -> m (HMap k v)
pop = parent

memberThisLevel :: Ord k => k -> HMap k v -> STM Bool
memberThisLevel k m = M.member k <$> entries m

insert :: Ord k => k -> v -> HMap k v -> STM ()
insert k v m = modifyEntries m $ M.insert k v

lookup :: Ord k => k -> HMap k v -> STM (Maybe v)
lookup k =
  cataM $ \case
    Top -> return Nothing
    HM p e -> do
      v <- M.lookup k <$> readTVar e
      return $ msum [v, p]

update :: (Show k, Ord k) => k -> v -> HMap k v -> STM ()
update k v m = do
  isThisLevel <- memberThisLevel k m
  if isThisLevel
    then insert k v m
    else maybe (fail $ "Does not exist: " ++ show k) (update k v) (parent m)
