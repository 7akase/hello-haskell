-- import Data.Monoid

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty


newtype Product a = Product { getProduct :: a}
instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product x) (Product y) = Product (x * y)
  -- Product x `mappend` Product y = Product (x * y)


newtype Sum a = Sum { getSum :: a}
instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  Sum a `mappend` Sum b = Sum (a + b)


newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid Any where
  mempty = Any False
  Any x `mappend` Any y = Any (x || y)

newtype All = All { getAll :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid All where
  mempty = All True
  All x `mappend` All y = All (x && y)

instance Monoid Ordering where
  mempty = EQ
  mappend LT _ = LT
  mappend EQ x = x
  mappend GT _ = GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

