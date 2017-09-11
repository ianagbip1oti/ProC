module ProC.Parser.ProC
  ( Parser
  , POperatorTable
  , isDefinedM
  , isOfTypeM
  , insertVariableM
  , parse
  ) where

import           ProC.Language

import           Data.Functor.Identity
import           Data.Set              (Set)
import qualified Data.Set              as Set

import           Text.Parsec           (ParseError, Parsec, getState,
                                        modifyState, runParser)
import           Text.Parsec.Expr

newtype ParseContext = ParseContext
  { variables :: Set (Identifier, PType)
  }

insertVariable :: Identifier -> PType -> ParseContext -> ParseContext
insertVariable v t c = ParseContext {variables = Set.insert (v, t) (variables c)}

isDefined :: Identifier -> ParseContext -> Bool
isDefined v c = any (\i -> v == fst i) (variables c)

isOfType :: PType -> Identifier -> ParseContext -> Bool
isOfType t i c = Set.member (i, t) (variables c)

empty :: ParseContext
empty = ParseContext Set.empty

type Parser = Parsec String ParseContext

insertVariableM :: Identifier -> PType -> Parser ()
insertVariableM v t = modifyState $ insertVariable v t

isDefinedM :: Identifier -> Parser Bool
isDefinedM v = isDefined v <$> getState

isOfTypeM :: PType -> Identifier -> Parser Bool
isOfTypeM t i = isOfType t i <$> getState

type POperatorTable a = OperatorTable String ParseContext Identity a

parse :: Parser a -> String -> Either ParseError a
parse p = runParser p empty "(Unknown)"
