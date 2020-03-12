module ParseHelp where

import qualified Data.ByteString as B

import Data.Word

consumeSubstr :: Int -> B.ByteString -> (B.ByteString, B.ByteString)
consumeSubstr i str = (B.drop i str, B.take i str)

splitSubstr :: Int -> B.ByteString -> [B.ByteString]
splitSubstr i str
    | B.length str < i = []
    | otherwise = let (s, a) = consumeSubstr i str in
        (a:splitSubstr i s)

type Array6 a = (a, a, a, a, a, a)
listArray6 :: Array6 a -> [a]
listArray6 (a, b, c, d, e, f) = [a, b, c, d, e, f]

makeArray6FromSlice :: B.ByteString -> Int -> Array6 Word8
makeArray6FromSlice a o = (a `B.index` (0+o), a `B.index` (1+o), a `B.index` (2+o), a `B.index` (3+o), a `B.index` (4+0), a `B.index` (5+o))