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

import           Control.Lens

import qualified ProC.Data.HierarchicalMap as HM

import           Text.Parsec               (ParseError, Parsec, getState,
                                            modifyState, putState, runParser)
import           Text.Parsec.Expr

type ParseContext = HM.HierarchicalMap Identifier PType

empty :: ParseContext
empty = HM.empty

insertVariable :: Identifier -> PType -> ParseContext -> ParseContext
insertVariable = HM.insert

isDefinedInCurrentScope :: Identifier -> ParseContext -> Bool
isDefinedInCurrentScope = HM.memberThisLevel

isOfType :: PType -> Identifier -> ParseContext -> Bool
isOfType t i c = HM.lookup i c == Just t

type Parser = Parsec String ParseContext

insertVariableM :: Identifier -> PType -> Parser ()
insertVariableM v t = modifyState $ insertVariable v t

isDefinedInCurrentScopeM :: Identifier -> Parser Bool
isDefinedInCurrentScopeM v = isDefinedInCurrentScope v <$> getState

isOfTypeM :: PType -> Identifier -> Parser Bool
isOfTypeM t i = isOfType t i <$> getState

enterBlockM :: Parser ()
enterBlockM = modifyState HM.push

exitBlockM :: Parser ()
exitBlockM = getState >>= HM.pop >>= putState

inBlockM :: Parser a -> Parser a
inBlockM p = enterBlockM *> p <* exitBlockM

type POperatorTable a = OperatorTable String ParseContext Identity a

parse :: Parser a -> String -> Either ParseError a
parse p = runParser p empty "(Unknown)"
