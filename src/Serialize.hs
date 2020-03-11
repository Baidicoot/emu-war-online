module Serialize where

import qualified Data.ByteString as B
import Data.Word
import Data.Bits

import Control.Monad
import Control.Applicative

type Get a = Maybe (a, B.ByteString)

newtype Getter a = Getter { runGet :: B.ByteString -> Maybe (a, B.ByteString) }

instance Functor Getter where
    fmap = liftM

instance Applicative Getter where
    pure = return
    (<*>) = ap

instance Alternative Getter where
    empty = Getter (\_ -> Nothing)
    a <|> b = Getter $ \s ->
        runGet a s <|> runGet b s

instance Monad Getter where
    return x = Getter (\s -> Just (x, s))
    p >>= k = Getter $ \s0 -> do
        (x, s1) <- runGet p s0
        runGet (k x) s1

instance MonadPlus Getter where
    mzero = empty
    mplus = (<|>)

putG :: B.ByteString -> Getter ()
putG s = Getter (\_ -> Just ((), s))

getG :: Getter B.ByteString
getG = Getter (\s -> Just (s, s))

class Serializable a where
    get :: Getter a
    put :: a -> B.ByteString

instance Serializable Word8 where
    put = B.singleton
    get = Getter $ \b ->
        if (B.length b > 0) then
            Just (B.head b, B.tail b)
        else Nothing

instance Serializable Word16 where
    put x = B.pack [
        fromIntegral (x `shiftR` 8)
        ,fromIntegral x]
    get = Getter $ \b ->
        if (B.length b > 1) then
            let [x, y] = B.unpack . B.take 2 $ b in Just
                (((fromIntegral x) `shiftL` 8) +
                (fromIntegral y), B.drop 2 b)
        else Nothing

--encode :: (Serializable a) => a -> B.ByteString

--decode :: (Serializable a) => Getter a