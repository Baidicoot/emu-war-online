module Serialize where

import qualified Data.ByteString as B
import Data.Word
import Data.Bits
import Numeric
import Utils
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

instance (BytesBE x) => Serializable (Wrapper x) where
    get = Getter $ (\(v, l) -> Just (wrap v, B.pack l)) . unbytes . B.unpack
    put = B.pack . bytes . unwrap

instance (Serializable x) => Serializable [x] where
    get = do
        l <- (get :: Getter (Wrapper Word16)) -- max list size = 65536
        let len = fromIntegral . unwrap $ l
        performN len decode
    put xs = B.append (put (wrap (fromIntegral . length $ xs :: Word16))) $ (B.concat . map encode) xs

encode :: (Serializable a) => a -> B.ByteString
encode a = (\b -> (put . wrap . (fromIntegral :: (Integral x) => x -> Word32) $ B.length b) `B.append` b) (put a)

decode :: (Serializable a) => Getter a
decode = Getter $ \b ->
    let (h, _) = (unbytes . B.unpack . B.take 4) b
        t = B.drop 4 b
        r = B.drop h t in do
            (x, _) <- runGet get (B.take h t)
            return (x, r)