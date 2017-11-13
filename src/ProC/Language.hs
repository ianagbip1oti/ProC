{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module ProC.Language
  ( module ProC.Language.PBlnExpression
  , module ProC.Language.PIntExpression
  , module ProC.Language.PStrExpression
  , module ProC.Language.PType
  , ProCProgram
  , Statement(..)
  ) where

import           ProC.Language.PBlnExpression
import           ProC.Language.PIntExpression
import           ProC.Language.PStrExpression
import           ProC.Language.PType

type ProCProgram = Statement

data Statement
  = Noop
  | Print PStrExpression
  | Block [Statement]
  | Seq [Statement]
  | Whl PBlnExpression
        [Statement]
  | PBlnVarDcl Identifier
               PBlnExpression
  | PBlnVarAss Identifier
               PBlnExpression
  | PIntVarDcl Identifier
               PIntExpression
  | PIntVarAss Identifier
               PIntExpression
  | PStrVarDcl Identifier
               PStrExpression
  | PStrVarAss Identifier
               PStrExpression
  deriving (Eq, Show)
