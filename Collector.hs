{-# LANGUAGE OverloadedStrings #-}
module Collector (runCollector
                 , prepareCollector
                 , CpuRoot) where

import Data.List (last)
import System.Directory (getDirectoryContents, doesDirectoryExist) 
import System.FilePath (combine, splitPath)
import Data.Char (isNumber, toUpper)
import Control.Monad (filterM, mapM, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Aeson 
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as LB

data CpuRoot = CpuRoot {name  :: String
                       , path :: FilePath
  } deriving (Show)
             
data CpuSample = CpuSample {cpu        :: String
                           , algorithm :: String
                           , minFreq   :: Int
                           , maxFreq   :: Int
                           , curFreq   :: Int
                           } deriving (Show)

instance ToJSON CpuSample where
  toJSON s =
    object ["cpu" .= cpu s
           ,"algorithm" .= algorithm s
           , "minFreq"  .= minFreq s
           , "maxFreq"  .= maxFreq s
           , "curFreq"  .= curFreq s]

runCollector :: [CpuRoot] -> Int -> IO ()
runCollector cpuRoots wsPort = do
  WS.runServer "0.0.0.0" wsPort $ (newWsConn cpuRoots)  
  
newWsConn :: [CpuRoot] -> WS.Request -> WS.WebSockets WS.Hybi00 ()
newWsConn cpuRoots rq = do
  WS.acceptRequest rq  
  forever $ do
    samples <- liftIO $ sample cpuRoots
    let json = encode samples
    WS.sendTextData json
    liftIO $ threadDelay 1000000

prepareCollector :: IO [CpuRoot]
prepareCollector = do
  paths <- collectDirectories
  mapM makeCpuRoot paths
  where 
    makeCpuRoot path =
      let dir = last $ splitPath path
      in return CpuRoot {name  = map toUpper dir
                        , path = path}
    
sample :: [CpuRoot] -> IO [CpuSample]
sample = mapM sampleCpu
      
collectDirectories :: IO [FilePath]
collectDirectories = do
  contents <- getDirectoryContents base
  paths    <- filterM cpuDir contents
  let paths' = map (combine base) paths
  filterM doesDirectoryExist paths'
  where
    base        = "/sys/devices/system/cpu"
    cpuDir path = return (length path > 3      &&
                          take 3 path == "cpu" &&
                          isNumber (path !! 3))
                  
sampleCpu :: CpuRoot -> IO CpuSample
sampleCpu cpu = do
  algorithm <- readAlgorithm
  minFreq   <- readMinFreq
  maxFreq   <- readMaxFreq
  curFreq   <- readCurFreq
  return CpuSample {cpu        = name cpu
                   , algorithm = algorithm
                   , minFreq   = read minFreq
                   , maxFreq   = read maxFreq
                   , curFreq   = read curFreq}
  where
    readAlgorithm = readFile $ combine (path cpu) "cpufreq/scaling_governor"
    readMinFreq   = readFile $ combine (path cpu) "cpufreq/cpuinfo_min_freq"
    readMaxFreq   = readFile $ combine (path cpu) "cpufreq/cpuinfo_max_freq"
    readCurFreq   = readFile $ combine (path cpu) "cpufreq/cpuinfo_cur_freq"    
    