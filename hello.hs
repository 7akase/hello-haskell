--  main = do
--    putStrLn "Hello, what's your name?"
--    name <- getLine
--    putStrLn ("Welcome!! " ++ name)
--

import Control.Monad.Reader

data Config = Config { verbose :: Int, debug :: Bool }

configToLevel :: Config -> Int
configToLevel config | debug config = 10
                     | otherwise = verbose config

outputLevel :: Reader Config [Int]
outputLevel = do
  config <- ask
  return [ 1 .. configToLevel config ]

output :: Int -> String -> Reader Config (Maybe String)
output level str = do
  ls <- outputLevel
  return (if level `elem` ls then Just str else Nothing)
