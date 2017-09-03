{-# LANGUAGE GADTs #-}
module ProC.Language where

import Control.Monad

data Expression a where
    StringLiteral :: String -> Expression String

instance (Show a) => Show (Expression a) where
  show (StringLiteral s) = show s


eval :: Expression a -> a
eval (StringLiteral s) = s

data Statement =
    Noop
    | Print (Expression String)
    | Seq [Statement]
    deriving (Show)
    
exec :: Statement -> IO ()
exec (Noop) = return ()
exec (Print s) = putStrLn $ eval s
exec (Seq ss) = forM_ ss exec