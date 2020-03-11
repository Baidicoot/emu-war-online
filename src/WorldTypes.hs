{-# LANGUAGE DeriveGeneric #-}
module WorldTypes where

import Common
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Word (Word8)
import qualified Data.Map.Strict as Map

type RegionIndex = (Word8, Word8, Word8)
type RefTile = (Word8, Word8)
newtype RefChunk = RefChunk [RefTile] deriving(Show, Generic)
data RefRegion = RefRegion [RefChunk] [PortalData] deriving(Show, Generic)
newtype PortalData = PortalData (Array6 Word8, Array6 Word8, Array6 Word8, Array6 Word8) deriving(Show, Generic)
newtype LoadedRegions = LoadedRegions (Map.Map (Word8, Word8, Word8) RefRegion) deriving(Show, Generic)

instance NFData RefChunk
instance NFData RefRegion
instance NFData PortalData
instance NFData LoadedRegions