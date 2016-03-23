import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Tree
import TertiaryTree

data Inst a = Inst {cell :: String, name :: Maybe a, from :: [Maybe a]}
 deriving (Show)

toInst :: Tree a -> Tree (Inst a)
toInst (Node x ts) = Node (Inst "pin_" (Just x) [])
                          (map (fmapTree toInst) ts)
 
-- usage : putStrLn . showTree . convert $ tertiarize numTree
convert :: Tree a -> Tree (Inst a)
convert = fmapTree toInst 

-- auxial functions for RTL generator

isAdder :: Tree a -> Bool
isAdder (Node _ ts) | length ts == 3 = True
                    | otherwise      = False

carry :: Tree (Inst a) -> [Tree (Inst a)]
carry t = foldrTree f [] t
  where f t' = let newTree = Node (rootLabel t'){cell = "co_"} [] 
               in if isAdder t' then (newTree:) else id

tertiarize :: Tree (Inst a) -> Tree (Inst a)
tertiarize (Node r ts0) = (Node r (newForest ts0))
  where
    newForest []            = [] -- Leaf
    newForest [t]           = [tertiarize t]
    newForest [t1,t2]       = fmap tertiarize [t1,t2]
    newForest [t1,t2,t3]    = fmap tertiarize [t1,t2,t3]
    newForest (t1:t2:t3:ts) = let newAdder = Node (Inst "sum_" Nothing [])  [t1,t2,t3]
                              in newAdder:(newForest ts)

wallace :: [Tree (Inst a)] -> [Tree (Inst a)]
wallace (x:[]) = [tertiarize x]
wallace (x:xs) = [tertiarize (Node (rootLabel x) (subForest x ++ c))]
                 ++ lsbs
  where c    = carry (head lsbs)
        lsbs = wallace xs

-- *****************************************************************************
--  configure
-- *****************************************************************************

nodePrefix :: String
nodePrefix = "N"

-- *****************************************************************************

sample :: IO()
sample = putStrLn . unlines $ showTree
         <$> fmap ((cell `mappend` (show . name)) . snd) <$> numberTree <$> xx
         where
           xx = wallace input 
           input = replicate 3 . convert $ foldr insertTree (singleton 10) [1..4] 
