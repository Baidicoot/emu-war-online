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