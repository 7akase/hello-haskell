import Control.Applicative
import Data.Tree
import qualified Data.Foldable as F

type Out  a = Tree (Inst a)
data Inst a = Inst {cell :: String, name :: Maybe a} deriving (Show)

wallace :: [Out a] -> [Out a]
wallace = undefined 

bindAdder :: Tree (Inst a) -> Tree (Inst a)
bindAdder = undefined

addSub :: Inst a -> Inst a -> Inst a
addSub = undefined

{-
bindAdder []            = []
bindAdder [n1]          = [n1]
bindAdder [n1,n2]       = [(Inst "HAS" Nothing)]
bindAdder (n1:n2:n3:ns) = [(Inst "FAS" Nothing)] ++ netsToAdders ns
-}
{-
carry :: Inst a -> Inst a
sumToCarry x | isAdder x = x {cell = "FAC"}

isAdder :: Inst a -> Bool
isAdder Inst {cell = cell}
  | cell == "FAS" || cell == "HAS" = True
  | otherwise = False
-}
