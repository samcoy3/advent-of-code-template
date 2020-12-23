module Util.Pair where

newtype Pair a = Pair {getPair :: (a, a)}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Pair where
  pure x = Pair (x, x)
  (Pair (f, g)) <*> (Pair (x, y)) = Pair (f x, g y)

instance (Semigroup a) => Semigroup (Pair a) where
  (Pair (a, b)) <> (Pair (c, d)) = Pair (a <> c, b <> d)

instance (Monoid a) => Monoid (Pair a) where
  mempty = Pair (mempty, mempty)

(<+>) :: Num a => Pair a -> Pair a -> Pair a
x <+> y = (+) <$> x <*> y
