import Control.Applicative
import Data.Monoid
import qualified Data.Foldable as F

data Inst a
  = Inst {cell :: String, input :: [Inst a], name :: Maybe a} deriving (Show)
{-
instance (Show a, Eq a) => Show (Inst a) where
  show x | name x == Nothing = cell x
         | otherwise         = cell x ++ show (Just (name x))
-}

instance F.Foldable Inst where
  foldMap f x | length (input x) == 0 = mempty 
              | otherwise             = F.foldMap mappend (F.foldMap f (input x))

type Bit a = [Inst a]
type ThisBit a = Bit a
type PrevBit a = Bit a

netsToAdders :: [Inst a] -> [Inst a]
netsToAdders []            = []
netsToAdders [n1]          = [n1]
netsToAdders [n1,n2]       = [(Inst "HAS" [n1,n2]    Nothing)]
netsToAdders (n1:n2:n3:ns) = [(Inst "FAS" [n1,n2,n3] Nothing)] ++ netsToAdders ns

sumToCarry :: Inst a -> Inst a
sumToCarry x@(Inst {cell = cell, name = name, input = xs })
  | cell == "FAS" || cell == "HAS" = x {cell = "FAC"} 

isAdder :: Inst a -> Bool
isAdder Inst {cell = cell} | cell == "FAS" || cell == "HAS" = True
                           | otherwise = False

-- >>> wallace (NET <$> [1..2]) $ wallace (NET <$> [3..11]) []
wallace :: ThisBit a -> PrevBit a -> [Inst a]
wallace lsb [] | length lsb >= 3 = wallace (netsToAdders lsb) []
               | otherwise       = lsb 
wallace msb lsb = netsToAdders (msb ++ (sumToCarry <$> (filter isAdder)  lsb))

a = replicate 3 $ Inst "pin" [] Nothing
b = replicate 8 $ Inst "pin" [] Nothing
