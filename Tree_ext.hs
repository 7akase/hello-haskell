module Tree_ext(

) where

import Data.Functor
import Data.Tree

-- newtype RoseTree a = RoseTree { getRoseTree :: Tree a} deriving (Show)-- for Tree
newtype RoseTree t = RoseTree { getRoseTree :: t } deriving (Show, Read)

instance Functor RoseTree where
  fmap f (RoseTree t) = RoseTree newTree
    where
      newTree = Node r' (ts' ++ map (fmap f) ts)
      ts  = subForest t
      r'  = rootLabel (f t)
      ts' = subForest (f t)

{-
instance Functor RoseTree where
  -- fmap f (RoseTree t) = RoseTree (Node (f (rootLabel t)) (map (fmap f) (subForest t))) 
  fmap f (RoseTree t) = RoseTree $ Node r' (ts' ++ map (fmap f) (subForest t))
    where
      Node r' ts' = f t
-}

node :: RoseTree a -> a
node (RoseTree (Node r ts)) = r

subs :: RoseTree a -> [RoseTree a]
subs (RoseTree (Node r ts)) = fmap RoseTree ts

{-
insertTree :: (Ord a) => a -> RoseTree a -> RoseTree a
insertTree x (RoseTree (Node r ts))
  | x == r = RoseTree (Node r ts)
  | x <  r = RoseTree (Node r (singleton x:ts))
  | x >  r = RoseTree (Node x (ts ++ [singleton r]))
    where 
      singleton x = Node x []
-}
