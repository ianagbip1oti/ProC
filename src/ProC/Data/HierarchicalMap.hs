{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

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

import           Prelude      hiding (lookup)

import           Control.Lens

import qualified Data.Map     as M
import           Data.Maybe

data HierarchicalMap k v = HierarchicalMap
  { _parent  :: Maybe (HierarchicalMap k v)
  , _entries :: M.Map k v
  }

makeLenses ''HierarchicalMap

empty :: HierarchicalMap k v
empty = HierarchicalMap Nothing M.empty

push :: HierarchicalMap k v -> HierarchicalMap k v
push m = parent ?~ m $ empty

pop :: Monad m => HierarchicalMap k v -> m (HierarchicalMap k v)
pop m = maybe (fail "Popped topmost level") return $ m ^. parent

keyThisLevel :: Ord k => k -> Traversal' (HierarchicalMap k v) v
keyThisLevel k = entries . at k . _Just

levelWith ::
     Ord k
  => k
  -> HierarchicalMap k v
  -> Traversal' (HierarchicalMap k v) (HierarchicalMap k v)
levelWith k m =
  case m ^? keyThisLevel k of
    Just _ -> id
    Nothing ->
      case m ^. parent of
        Just p  -> parent . _Just . levelWith k p
        Nothing -> id

key :: Ord k => k -> HierarchicalMap k v -> Traversal' (HierarchicalMap k v) v
key k m = levelWith k m . keyThisLevel k

memberThisLevel :: Ord k => k -> HierarchicalMap k v -> Bool
memberThisLevel k m = isJust $ m ^. entries ^. at k

insert :: Ord k => k -> v -> HierarchicalMap k v -> HierarchicalMap k v
insert k v = entries . at k ?~ v

update :: Ord k => k -> v -> HierarchicalMap k v -> HierarchicalMap k v
update k v m = key k m .~ v $ m

lookup :: Ord k => k -> HierarchicalMap k v -> Maybe v
lookup k m = m ^? key k m
