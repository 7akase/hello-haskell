module TertiaryTree(
  tertiarize, treeInsert
  ) where

import Control.Applicative
import Data.Tree

tertiarize :: Tree (Maybe a) -> Tree (Maybe a)
tertiarize (Node r ts0) = (Node r (newForest ts0))
  where
    newForest [] = [] -- Leaf
    newForest [t] = [tertiarize t]
    newForest [t1,t2] = fmap tertiarize [t1,t2]
    newForest [t1,t2,t3] = fmap tertiarize [t1,t2,t3]
    newForest (t1:t2:t3:ts) = newForest ((tertiarize (Node Nothing [t1,t2,t3])):ts) 

singleton :: a -> Tree a
singleton x = Node x []

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x (Node r ts)
  | x == r = Node x ts
  | x <  r = Node r ((singleton x):ts)
  | x >  r = Node x (ts ++ [(singleton r)])


nums = Just <$> [1..20]
numTree = foldr treeInsert (singleton (Just 4)) nums
main = do
  putStrLn $ drawTree (fmap show numTree)
test = do
  putStrLn . drawTree . (fmap show) . tertiarize $ numTree
