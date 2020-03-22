module Parsing.World.Parser where

import Types.World.World
import Parsing.ParseUtils.ParseUtils
import Parsing.ParseUtils.Serialize
import Types.World.World
import qualified Data.ByteString as B
import Data.Word
import Data.Vector.Storable.ByteString

serializePortal :: PortalData -> [Word8]
serializePortal (PortalData (a, b, c, d)) = concatMap listArray6 [a, b, c, d]

deserializePortal :: B.ByteString -> PortalData
deserializePortal str = let [a, b, c, d] = map (makeArray6FromSlice str) [0, 6, 12, 18] in
    PortalData (a, b, c, d)

getRegion :: B.ByteString -> Maybe (Region, B.ByteString)
getRegion bstr =
    let npd = fromIntegral (B.head bstr)
        str = B.tail bstr
        tile_raw = B.take 131072 str
        tiles = byteStringToVector tile_raw
        portal_indexes = if npd == 0 then [] else [x*24 | x <- [1..npd]]
        portal_raw = B.drop 131072 str
        pds = map (deserializePortal . B.take 24 . (\n -> B.drop n portal_raw)) portal_indexes
        tail_raw = B.drop (npd * 24) portal_raw
    in
        Just (Region (RegionTileData tiles) pds, tail_raw)

putRegion :: Region -> B.ByteString
putRegion (Region (RegionTileData ti) pd) =
    let tile_raw = vectorToByteString ti
        h = fromIntegral (length pd)
        pdr = B.concat . map (B.pack . serializePortal) $ pd
    in
        h `B.cons` tile_raw `B.append` pdr

instance Serializable Region where
    get = Getter getRegion
    put = putRegion