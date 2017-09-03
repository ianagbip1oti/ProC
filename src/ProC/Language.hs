{-# LANGUAGE GADTs #-}
module ProC.Language where

import Control.Monad

import Data.List

data Expression a where
    StringLiteral :: String -> Expression String

instance (Show a) => Show (Expression a) where
  show (StringLiteral s) = "\"" ++ s


eval :: Expression a -> a
eval (StringLiteral s) = s

data Statement where
    Noop :: Statement
    Print :: Expression String -> Statement
    Seq :: [Statement] -> Statement
    
instance Show Statement where
  show Noop = ""
  show (Print s) = "print(" ++ show s ++ ")"
  show (Seq s) = intercalate ";" $ fmap show s
    
exec :: Statement -> IO ()
exec (Noop) = return ()
exec (Print s) = putStrLn $ eval s
exec (Seq ss) = forM_ ss exec