{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module ProC.Parser.ProC
  ( Parser
  , POperatorTable
  , isDefinedInCurrentScopeM
  , isOfTypeM
  , insertVariableM
  , enterBlockM
  , exitBlockM
  , inBlockM
  , parse
  ) where

import           ProC.Language

import Control.Lens

import qualified Data.Map as M
import Data.Maybe

import           Text.Parsec           (ParseError, Parsec, getState,
                                        modifyState, putState, runParser)
import           Text.Parsec.Expr

data ParseContext = ParseContext
  { _parent    :: Maybe ParseContext
  , _variables :: M.Map Identifier PType
  }

makeLenses ''ParseContext

varThisContext :: Identifier -> Traversal' ParseContext PType
varThisContext idn = variables . at idn . _Just

contextWith :: Identifier -> ParseContext -> Traversal' ParseContext ParseContext
contextWith idn ctx =
  case ctx ^? varThisContext idn of
    Just _ -> id
    Nothing ->
      case ctx ^. parent of
        Just p  -> parent . _Just . contextWith idn p
        Nothing -> id

variable :: Identifier -> ParseContext -> Traversal' ParseContext PType
variable idn ctx = contextWith idn ctx . varThisContext idn

insertVariable :: Identifier -> PType -> ParseContext -> ParseContext
insertVariable v t = variables . at v ?~ t

isDefinedInCurrentScope :: Identifier -> ParseContext -> Bool
isDefinedInCurrentScope v c = isJust $ c ^? varThisContext v

isOfType :: PType -> Identifier -> ParseContext -> Bool
isOfType t i c = c ^? variable i c == Just t

empty :: ParseContext
empty = ParseContext Nothing M.empty

enterBlock :: ParseContext -> ParseContext
enterBlock c = ParseContext {_parent = Just c, _variables = M.empty}

exitBlock :: (Monad m) => ParseContext -> m ParseContext
exitBlock c = maybe (fail "Attempt to exit top level block") return (c ^. parent)

type Parser = Parsec String ParseContext

insertVariableM :: Identifier -> PType -> Parser ()
insertVariableM v t = modifyState $ insertVariable v t

isDefinedInCurrentScopeM :: Identifier -> Parser Bool
isDefinedInCurrentScopeM v = isDefinedInCurrentScope v <$> getState

isOfTypeM :: PType -> Identifier -> Parser Bool
isOfTypeM t i = isOfType t i <$> getState

enterBlockM :: Parser ()
enterBlockM = modifyState enterBlock

exitBlockM :: Parser ()
exitBlockM = getState >>= exitBlock >>= putState

inBlockM :: Parser a -> Parser a
inBlockM p = enterBlockM *> p <* exitBlockM

type POperatorTable a = OperatorTable String ParseContext Identity a

parse :: Parser a -> String -> Either ParseError a
parse p = runParser p empty "(Unknown)"
