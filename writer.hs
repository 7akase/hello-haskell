import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) }
instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x, v)) >>= f = let (Writer (x', v')) = f x
                            in  Writer (x', v `mappend` v')

