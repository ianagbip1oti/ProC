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

import qualified ProC.Data.HierarchicalMap as HM

import           Control.Concurrent.STM
import           Control.Monad.IO.Class

import           Text.Parsec               (ParseError, ParsecT, getState,
                                            putState, runParserT)
import           Text.Parsec.Expr

type ParseContext = HM.HierarchicalMap Identifier PType

empty :: MonadIO m => m ParseContext
empty = liftIO (atomically HM.empty)

insertVariable :: MonadIO m => Identifier -> PType -> ParseContext -> m ()
insertVariable idn t = liftIO . atomically . HM.insert idn t

isDefinedInCurrentScope :: MonadIO m => Identifier -> ParseContext -> m Bool
isDefinedInCurrentScope idn = liftIO . atomically . HM.memberThisLevel idn

isOfType :: MonadIO m => PType -> Identifier -> ParseContext -> m Bool
isOfType t i c = (==) (Just t) <$> (liftIO . atomically . HM.lookup i) c

type Parser = ParsecT String ParseContext IO

insertVariableM :: Identifier -> PType -> Parser ()
insertVariableM v t = getState >>= insertVariable v t

isDefinedInCurrentScopeM :: Identifier -> Parser Bool
isDefinedInCurrentScopeM v = getState >>= isDefinedInCurrentScope v

isOfTypeM :: PType -> Identifier -> Parser Bool
isOfTypeM t i = getState >>= isOfType t i

enterBlockM :: Parser ()
enterBlockM = getState >>= liftIO . atomically . HM.push >>= putState

exitBlockM :: Parser ()
exitBlockM = getState >>= liftIO . atomically . HM.pop >>= putState

inBlockM :: Parser a -> Parser a
inBlockM p = enterBlockM *> p <* exitBlockM

type POperatorTable a = OperatorTable String ParseContext IO a

parse :: Parser a -> String -> IO (Either ParseError a)
parse p s = empty >>= \rc -> runParserT p rc "(Unknown)" s
