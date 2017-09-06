module ProC.Parser.ProC where

import ProC.Language

import Data.Functor.Identity
import Data.Set (Set)
import qualified Data.Set as Set

import Text.Parsec
import Text.Parsec.Expr

data ParseContext = ParseContext { variables :: Set String }

empty :: ParseContext
empty = ParseContext Set.empty

type Parser = Parsec String ParseContext

type POperatorTable a = OperatorTable String ParseContext Identity a

parse :: Parser ProCProgram -> String -> Either ParseError ProCProgram
parse p = runParser p empty "(Unknown)"

