{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ProC.Language where

import Control.Monad

data StringExpression =
  StringLiteral String | StringConcat StringExpression StringExpression

eval :: StringExpression -> String
eval (StringLiteral s) = s
eval (StringConcat l r) = eval l ++ eval r

data Statement where
    Noop :: Statement
    Print :: StringExpression -> Statement
    Seq :: [Statement] -> Statement
        
exec :: Statement -> IO ()
exec (Noop) = return ()
exec (Print s) = putStrLn $ eval s
exec (Seq ss) = forM_ ss exec