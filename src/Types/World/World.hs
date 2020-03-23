{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
module Types.World.World where

import Parsing.ParseUtils.ParseUtils
import Utils.Utils

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.ByteString
import qualified Data.Map.Strict as Map

data Tile = Tile Int (Maybe Int)

tile :: Word8 -> Word8 -> Tile
tile a 255 = Tile (fromIntegral a) Nothing
tile a x = Tile (fromIntegral a) (Just (fromIntegral x))

untile :: Tile -> (Word8, Word8)
untile (Tile a Nothing) = (fromIntegral a, 255)
untile (Tile a (Just x)) = (fromIntegral a, fromIntegral x)

newtype RegionTileData = RegionTileData (VS.Vector Word8) deriving(Generic)

(!?) :: RegionTileData -> Int -> Maybe Tile
(RegionTileData x) !? w =
    let i = w * 2 in do
        a <- x VS.!? i
        b <- x VS.!? (i + 1)
        return $ tile a b

(//) :: RegionTileData -> [(Int, Tile)] -> RegionTileData
(RegionTileData x) // iv =
    let iv' = concatMap makeWord8Pair iv in
        RegionTileData (x VS.// iv')
    where
        makeWord8Pair (i, t) = let (a, b) = untile t in
            [(i * 2, a), (i * 2 + 1, b)]

newtype PortalData = PortalData (Coord, Coord, Coord, Coord) deriving(Generic)
data Region = Region RegionTileData [PortalData] deriving(Generic) -- parsed form to be used in-game

type RegionIndex = (Word8, Word8, Word8)
type Coord = Array5 Word8

regionIndex :: Coord -> RegionIndex
regionIndex (a, b, c, _, _) = (a, b, c)

type RegionMap = Map.Map RegionIndex
type RegionBlobTable = RegionMap Region

instance NFData RegionTileData
instance NFData PortalData
instance NFData Region