module ProC.Language where

data Statement = Print String
    deriving (Show)
    
exec :: Statement -> IO ()
exec (Print s) = putStrLn s