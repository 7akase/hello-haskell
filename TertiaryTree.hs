module TertiaryTree(
  tertiarize, treeInsert, singleton,
  fmap, fmapT, foldrT, F.foldl, F.foldr,
  showT) where

import Control.Applicative
import Data.Tree
import Data.Maybe
import Data.Monoid
import qualified Data.Foldable as F

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

fmapT :: (Tree a -> Tree b) -> Tree a -> Tree b
fmapT f t@(Node x ts) = Node x' (map (fmapT f) ts)
  where (Node x' ts') = f t 

foldrT :: Monoid m => (Tree a -> m -> m) -> m -> Tree a -> m
foldrT f z t@(Node r []) = f t z
foldrT f z t@(Node r ts) = (f t mempty) `mappend` (foldr mappend mempty (fmap (foldrT f mempty) ts))

showT :: (Show a) => Tree a -> String
showT = drawTree . (fmap show)


nums = Just <$> [1..20]
numTree = F.foldr treeInsert (singleton (Just 4)) nums

main = do
  putStrLn $ drawTree (fmap show numTree)
test = do
  putStrLn . drawTree . (fmap show) . tertiarize $ numTree

