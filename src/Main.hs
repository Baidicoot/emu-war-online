module Main where

import Control.DeepSeq
import LoadWorld
import Serialize
import WorldTypes
import Text.Printf
import System.CPUTime
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

diff :: Integer -> Integer -> IO ()
diff start end = do
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "executed in: %0.5f sec\n" (diff :: Double)

deser :: IO ()
deser = do
    putStrLn "starting test..."
    str <- B.readFile "log.re"
    case deserializeRefRegion str of
        Just x -> do
            start <- getCPUTime
            end <- x `deepseq` getCPUTime
            diff start end
        Nothing -> putStrLn "test failed"

region :: RefRegion
region = let (Just x) = deserializeRefRegion . serializeRefRegion $ (RefRegion (replicate 256 ch) []) in x
    where
        ch = RefChunk (replicate 256 (0, 255))

regions :: LoadedRegions
regions = LoadedRegions Map.empty

load :: IO ()
load = do
    putStrLn "starting test"
    start <- getCPUTime
    mMap <- loadRegion (0, 0, 0) "log.re" regions
    end <- getCPUTime
    diff start end
    return ()

ser :: IO ()
ser = do
    putStrLn "starting test"
    start <- region `deepseq` getCPUTime
    let serial = serializeRefRegion region
    end <- serial `deepseq` getCPUTime
    diff start end