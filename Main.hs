-- Copyright (C) 2012, Patrik Sandahl
-- sneakingcatsw@gmail.com
module Main where

import Collector
import Renderer
import Happstack.Server
import Data.Char (isNumber)
import Control.Monad (msum)
import Control.Concurrent (forkIO)
import System.Environment (getArgs, getProgName)

data Config = Config {httpPort :: Int
                     , wsPort :: Int}
              | Help
              deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case makeConfig args of
    Just (Config httpPort wsPort) -> startServices httpPort wsPort
    Just Help                     -> help
    Nothing                       -> help

startServices :: Int -> Int -> IO ()
startServices httpPort wsPort = do
  cpuRoots  <- prepareCollector
  forkIO $ runCollector cpuRoots wsPort
  let frontPage = render $ length cpuRoots
  simpleHTTP nullConf {port = httpPort} $ 
    msum [dir "js" $ serveDirectory DisableBrowsing [] "js"
         , ok $ toResponse frontPage]

help :: IO()
help = do
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " [options]")
  putStrLn "Options:"
  putStrLn "  -http <port>     Listening port for http (default: 8000)"
  putStrLn "  -ws <port>       Listening port for ws (default: 9160)"
  putStrLn "  -h               Show this information"

makeConfig :: [String] -> Maybe Config
makeConfig [] = Just Config {httpPort = 8000, wsPort = 9160}
makeConfig ["-h"] = Just Help
makeConfig ["-http", http] 
  | isIntConvertible http = Just Config {httpPort = read http, wsPort = 9160}
  | otherwise            = Nothing
makeConfig ["-ws", ws]
  | isIntConvertible ws = Just Config {httpPort = 8000, wsPort = read ws}
  | otherwise           = Nothing
makeConfig ["-http", http, "-ws", ws]
    | isIntConvertible http && isIntConvertible ws =
      Just Config {httpPort = read http, wsPort = read ws}
    | otherwise                                   = Nothing  
makeConfig ["-ws", ws, "-http", http]
  | isIntConvertible ws && isIntConvertible http =
    Just Config {httpPort = read http, wsPort = read ws}
  | otherwise                                   = Nothing  
makeConfig _ = Nothing

isIntConvertible :: String -> Bool
isIntConvertible str = "" == filter (not . isNumber) str
