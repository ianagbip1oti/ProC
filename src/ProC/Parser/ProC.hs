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

import           Data.Functor.Identity
import           Data.Set              (Set)
import qualified Data.Set              as Set

import           Text.Parsec           (ParseError, Parsec, getState,
                                        modifyState, putState, runParser)
import           Text.Parsec.Expr

data ParseContext = ParseContext
  { parent    :: Maybe ParseContext
  , variables :: Set (Identifier, PType)
  }

insertVariable :: Identifier -> PType -> ParseContext -> ParseContext
insertVariable v t c =
  ParseContext {parent = parent c, variables = Set.insert (v, t) (variables c)}

isDefinedInCurrentScope :: Identifier -> ParseContext -> Bool
isDefinedInCurrentScope v c = any (\i -> v == fst i) (variables c)

isOfType :: PType -> Identifier -> ParseContext -> Bool
isOfType t i c = inCurrentScope || maybe False (isOfType t i) (parent c)
  where
    inCurrentScope = Set.member (i, t) (variables c)

empty :: ParseContext
empty = ParseContext Nothing Set.empty

enterBlock :: ParseContext -> ParseContext
enterBlock c = ParseContext {parent = Just c, variables = Set.empty}

exitBlock :: (Monad m) => ParseContext -> m ParseContext
exitBlock c = maybe (fail "Attempt to exit top level block") return (parent c)

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
