module Main where

import Types.Entity.Entity
import Types.World.World
import Parsing.World.Parser
import Logic.Game.Game

import Text.Printf

diff :: Integer -> Integer -> IO ()
diff start end = do
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "executed in: %0.5f sec\n" (diff :: Double)