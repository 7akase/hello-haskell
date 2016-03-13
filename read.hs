-- import Control.Monad.Read

newtype Reader e a = Reader {runReader :: e -> a}

instance Monad (Reader e) where
  ret x = Reader (\x -> x)
  (Reader f) `bind` g = Reader (\e -> runReader (g (f e)) e)

ask :: Reader e a
ask = Reader id

local :: (e -> e) -> Reader e a -> Reader e a
local f c = Reader (\e -> runReader c (f e))

