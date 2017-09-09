module ProC.Parser.ProC
  ( Parser
  , POperatorTable
  , isDefinedM
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
  { variables :: Set Identifier
  }

insertVariable :: Identifier -> ParseContext -> ParseContext
insertVariable v c = ParseContext {variables = Set.insert v (variables c)}

isDefined :: Identifier -> ParseContext -> Bool
isDefined v c = Set.member v (variables c)

empty :: ParseContext
empty = ParseContext Set.empty

type Parser = Parsec String ParseContext

insertVariableM :: Identifier -> Parser ()
insertVariableM v = modifyState $ insertVariable v

isDefinedM :: Identifier -> Parser Bool
isDefinedM v = isDefined v <$> getState

type POperatorTable a = OperatorTable String ParseContext Identity a

parse :: Parser a -> String -> Either ParseError a
parse p = runParser p empty "(Unknown)"
