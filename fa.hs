import Control.Applicative

data Net a
  = FAC (Net a) (Net a) (Net a) -- Full Adder Carry
  | FAS (Net a) (Net a) (Net a) -- Full Adder Sum
  | HAC (Net a) (Net a)          -- Half Adder Carry
  | HAS (Net a) (Net a)          -- Half Adder Sum
  | NET a                          -- Net
  deriving (Show)

type ThisBit a = [Net a]
type PrevBit a = [Net a]

netsToAdders :: [Net a] -> [Net a]
netsToAdders []            = []
netsToAdders [n1]          = [n1]
netsToAdders [n1,n2]       = [HAS n1 n2]
netsToAdders (n1:n2:n3:ns) = [FAS n1 n2 n3] ++ netsToAdders ns

isFA :: Net a -> Bool
isFA (FAS _ _ _) = True
isFA _           = False

isHA :: Net a -> Bool
isHA (HAS _ _) = True
isHA _         = False

isNET :: Net a -> Bool
isNET (NET _) = True
isNet _       = False

isAdder :: Net a -> Bool
isAdder x = isFA x || isHA x

sumToCarry :: Net a -> Net a
sumToCarry (FAS n1 n2 n3) = FAC n1 n2 n3
sumToCarry (HAS n1 n2)    = HAC n1 n2

-- >>> wallace (NET <$> [1..2] $ wallace (NET <$> [3..11]) []
wallace :: ThisBit a -> PrevBit a -> [Net a]
wallace as [] | length as >= 3 = wallace (netsToAdders as) []
              | otherwise      = as
wallace as bs = netsToAdders (as ++ (sumToCarry <$> (filter isAdder)  bs))
