module Utils where

type Array6 a = (a, a, a, a, a, a)
listArray6 :: Array6 a -> [a]
listArray6 (a, b, c, d, e, f) = [a, b, c, d, e, f]

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