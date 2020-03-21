module Utils.Numeric where

import Data.Word
import Data.Bits

bytes_ :: (Integral x, Bits x) => Int -> x -> [Word8]
bytes_ i' x' = calcBytes x' (i'-1)
    where
        calcBytes x 0 = [fromIntegral x]
        calcBytes x i = (fromIntegral (x `shiftR` (i * 8))):(calcBytes x (i - 1))

unbytes_ :: (Integral x, Bits x) => Int -> [Word8] -> (x, [Word8])
unbytes_ i' x' = (calcSum x' (i'-1), drop i' x')
    where
        calcSum (x:xs) i = (fromIntegral (x `shiftL` (8 * i))) + calcSum xs (i - 1)
        calcSum _ 0 = 0
        calcSum [] _ = 0

class (Integral x, Bits x) => BytesBE x where -- big-endian byte conversion
    bytes :: x -> [Word8]
    bytes = bytes_ 8
    unbytes :: [Word8] -> (x, [Word8])
    unbytes = unbytes_ 8

instance BytesBE Word8 where
    bytes x = [x]
    unbytes b = (head b, tail b)

instance BytesBE Word16 where
    bytes = bytes_ 2
    unbytes = unbytes_ 2

instance BytesBE Word32 where
    bytes = bytes_ 4
    unbytes = unbytes_ 4

instance BytesBE Word64
instance BytesBE Int