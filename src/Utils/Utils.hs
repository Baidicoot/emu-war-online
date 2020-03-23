module Utils.Utils where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.List (groupBy)

type Array5 a = (a, a, a, a, a)
listArray5 :: Array5 a -> [a]
listArray5 (a, b, c, d, e) = [a, b, c, d, e]

newtype Wrapper a = Wrapper a
unwrap :: Wrapper a -> a
unwrap (Wrapper a) = a
wrap :: a -> Wrapper a
wrap = Wrapper

performN :: Monad m => Int -> m t -> m [t]
performN 0 _ = return []
performN n m =
  do x  <- m
     xs <- performN (n-1) m
     return $ x:xs

class Functor f => Zippable f where
  zip' :: f a -> f b -> f (a, b)

  zip3' :: f a -> f b -> f c -> f (a, b, c)
  zip3' a b c = fmap (\((a, b), c) -> (a, b, c)) (zip' (zip' a b) c)

  zip4' :: f a -> f b -> f c -> f d -> f (a, b, c, d)
  zip4' a b c d = fmap (\((a, b, c), d) -> (a, b, c, d)) (zip' (zip3' a b c) d)

  zip5' :: f a -> f b -> f c -> f d -> f e -> f (a, b, c, d, e)
  zip5' a b c d e = fmap (\((a, b, c, d), e) -> (a, b, c, d, e)) (zip' (zip4' a b c d) e)

zipMapsUnion :: (Ord k) => Map.Map k a -> Map.Map k b -> Map.Map k (Maybe a, Maybe b)
zipMapsUnion a b =
  let keyL = Set.toList $ (Map.keysSet a) `Set.union` (Map.keysSet b) in
    Map.fromList [(k, (a Map.!? k, b Map.!? k)) | k <- keyL]

zipMapsInter :: (Ord k) => Map.Map k a -> Map.Map k b -> Map.Map k (a, b)
zipMapsInter a b =
  let keyL = Set.toList $ (Map.keysSet a) `Set.intersection` (Map.keysSet b) in
    Map.fromList [(k, (a Map.! k, b Map.! k)) | k <- keyL]

instance Ord k => Zippable (Map.Map k) where
  zip' = zipMapsInter

groupKeyList :: (Eq k) => [(k, a)] -> [(k, [a])]
groupKeyList = map (\xs@((k, _):_) -> (k, map (\(_, a) -> a) xs)) . groupBy (\(k, _) (k', _) -> k == k')

fromListToLists :: (Ord k) => [(k, a)] -> Map.Map k [a]
fromListToLists xs = Map.fromList xs'
  where xs' = groupKeyList xs