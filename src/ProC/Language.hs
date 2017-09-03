module ProC.Language where

import Control.Monad

data Statement =
    Noop
    | Print String
    | Seq [Statement]
    deriving (Show)
    
exec :: Statement -> IO ()
exec (Noop) = return ()
exec (Print s) = putStrLn s
exec (Seq ss) = forM_ ss exec