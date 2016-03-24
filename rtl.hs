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
 
-- usage : putStrLn . showTree . convert $ tertiary numTree
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

tertiary :: Tree (Inst a) -> Tree (Inst a)
tertiary (Node r ts0) = (Node r (newForest ts0))
  where
    newForest []                  = []
    newForest ts | length ts <= 3 = fmap tertiary ts
                 | otherwise      = let x = Node (Inst "sum_" Nothing [])
                                                 (take 3 ts)
                                    in newForest $ x:(drop 3 ts)

binary_1 :: Tree (Inst a) -> Tree (Inst a)
binary_1 = undefined

wallace :: [Tree (Inst a)] -> [Tree (Inst a)]
wallace (x:[]) = [tertiary x]
wallace (x:xs) = [tertiary (Node (rootLabel x) (subForest x ++ c))]
                 ++ lsbs
  where c    = carry (head lsbs)
        lsbs = wallace xs

nameTree :: (Show a, Show b) => Tree (a, Inst b) -> Tree (Inst String)
nameTree t = fmap nameNode t

nameNode :: (Show a, Show b) => (a, Inst b) -> Inst String
nameNode (a, i@(Inst c Nothing  from)) = Inst c (Just ("N_" ++ show a)) [] 
nameNode (a, i@(Inst c (Just n) from)) = Inst c (Just (show n)) []

-- *****************************************************************************
--  configure
-- *****************************************************************************

nodePrefix :: String
nodePrefix = "N"

-- *****************************************************************************

sample3 = putStrLn . unlines $ showTree <$>
          fmap ((Just . cell) `mappend` name) <$> nameTree <$> numberTree <$> xx
            where
              xx = wallace input
              input = replicate 2 . convert $ foldr insertTree (singleton 100) [1..10] 

sample :: IO()
sample = putStrLn . unlines $ showTree
         <$> fmap ((cell `mappend` (show . name)) . snd) <$> numberTree <$> xx
         where
           xx = wallace input 
           input = replicate 2 . convert $ foldr insertTree (singleton 10) [1..10] 

sample2 :: IO()
sample2 = putStrLn . unlines $ showTree
          <$> fmap ((cell `mappend` (show . name)) . snd) <$> numberTree <$> xx
          where
            xx = wallace input 
            input = replicate 2 . convert $ foldr insertTree (singleton 100) [1..10] 
