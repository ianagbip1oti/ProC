{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ProC.Language where

import Control.Monad

class Eval exp res | exp -> res where
  eval :: exp -> res

data NumericExpression = IntLiteral Integer

instance Eval NumericExpression Integer where
    eval (IntLiteral i) = i

data StringExpression =
  StringLiteral String | StringConcat StringExpression StringExpression

instance Eval StringExpression String where
    eval (StringLiteral s) = s
    eval (StringConcat l r) = eval l ++ eval r

data Statement where
    Noop :: Statement
    Print :: (Eval e s, Show s) => e -> Statement
    Seq :: [Statement] -> Statement
        
exec :: Statement -> IO ()
exec (Noop) = return ()
exec (Print s) = putStrLn . show $ eval s
exec (Seq ss) = forM_ ss exec