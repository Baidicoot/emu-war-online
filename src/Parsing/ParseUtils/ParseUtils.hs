module Parsing.ParseUtils.ParseUtils where

import qualified Data.ByteString as B

import Data.Word
import Utils.Utils

consumeSubstr :: Int -> B.ByteString -> (B.ByteString, B.ByteString)
consumeSubstr i str = (B.drop i str, B.take i str)

splitSubstr :: Int -> B.ByteString -> [B.ByteString]
splitSubstr i str
    | B.length str < i = []
    | otherwise = let (s, a) = consumeSubstr i str in
        (a:splitSubstr i s)

makeArray5FromSlice :: B.ByteString -> Int -> Array5 Word8
makeArray5FromSlice a o = (a `B.index` (0+o), a `B.index` (1+o), a `B.index` (2+o), a `B.index` (3+o), a `B.index` (4+0))