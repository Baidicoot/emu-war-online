module LoadWorld where

import Control.DeepSeq
import WorldTypes
import ParseHelp
import Serialize
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString as B
import Data.Word
import Control.Monad
import Data.Maybe
import Data.Bits

import System.IO

serializePortal :: PortalData -> [Word8]
serializePortal (PortalData (a, b, c, d)) = concatMap listArray6 [a, b, c, d]

deserializePortal :: B.ByteString -> Int -> PortalData
deserializePortal str o = let [a, b, c, d] = map (makeArray6FromSlice str) [o, o+6, o+12, o+18] in
    PortalData (a, b, c, d)

unwrapRefChunk :: RefChunk -> [RefTile]
unwrapRefChunk (RefChunk x) = x

serializeRefChunk :: RefChunk -> B.ByteString
serializeRefChunk = B.pack . foldr (\(x, y) -> \ls -> x:y:ls) [] . unwrapRefChunk

deserializeRefChunk :: B.ByteString -> Int -> RefChunk
deserializeRefChunk str off = RefChunk $ do
    tile <- [0,2..510]
    return (str `B.index` (tile + off), str `B.index` (tile + off + 1))

serializeRefRegion :: RefRegion -> B.ByteString
serializeRefRegion (RefRegion ch pd) =
    let h = fromIntegral (length pd)
        bmp = B.concat . map serializeRefChunk $ ch
        pdr = B.pack . concat . map serializePortal $ pd
    in
        h `B.cons` bmp `B.append` pdr

deserializeRefRegion :: B.ByteString -> Maybe (RefRegion, B.ByteString)
deserializeRefRegion bsr =
    let npd = fromIntegral (B.head bsr)
        str = B.drop 1 bsr
        chunks = [0,512..130560]
        portals = if npd == 0 then [] else [x*24 + 131072 | x <- [1..npd]]
        len = 131072 + (npd * 24)
    in
        Just (RefRegion (map (deserializeRefChunk str) chunks) (map (deserializePortal str) portals), B.drop len str)

instance Serializable RefRegion where
    get = Getter deserializeRefRegion
    put = serializeRefRegion