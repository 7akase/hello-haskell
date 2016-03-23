module TertiaryTree(
  insertTree, singleton,
  fmapTree, foldrTree, zipTree,
  showTree, numberTree) where

import Data.Tree

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Traversable
import qualified Data.Foldable as F

{-#LANGUAGE DeriveTraversable #-}


singleton :: a -> Tree a
singleton x = Node x []

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x (Node r ts)
  | x == r = Node x ts
  | x <  r = Node r ((singleton x):ts)
  | x >  r = Node x (ts ++ [(singleton r)])

fmapTree :: (Tree a -> Tree b) -> Tree a -> Tree b
fmapTree f t@(Node x ts) = Node x' (map (fmapTree f) ts)
  where (Node x' ts') = f t 

foldrTree :: Monoid m => (Tree a -> m -> m) -> m -> Tree a -> m
foldrTree f z t@(Node r []) = f t z
foldrTree f z t@(Node r ts) = (f t mempty) `mappend` (foldr mappend mempty (fmap (foldrTree f mempty) ts))

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree (Node r1 xs1) (Node r2 xs2) = Node (r1,r2) (zipTree <$> xs1 <*> xs2) 

showTree :: (Show a) => Tree a -> String
showTree = drawTree . (fmap show)

numberTree :: Tree a -> Tree (Integer, a)
numberTree = snd . mapAccumL (\acc x -> (acc + 1, (acc + 1, x))) 0

nums = Just <$> [1..20]
numTree = F.foldr insertTree (singleton (Just 4)) nums

main = do
  putStrLn $ drawTree (fmap show numTree)

