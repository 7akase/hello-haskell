import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Tree
import TertiaryTree

data Inst a = Inst {cell :: String, name :: Maybe a, from :: [Maybe a]}
 deriving (Show)

toInst :: Tree (Maybe a) -> Tree (Maybe (Inst a))
toInst (Node Nothing  ts) = Node (Just (Inst "" Nothing 
                                             (fmap (\(Node r _) -> r) ts)))
                                 (fmap (fmapTree toInst) ts)
toInst (Node (Just x) ts) = Node (Just (Inst "" (Just x)
                                             (fmap (\(Node r _) -> r) ts)))
                                 (fmap (fmapTree toInst) ts)
 
-- usage : putStrLn . showTree . convert $ tertiarize numTree
convert :: Tree (Maybe a) -> Tree (Maybe (Inst a))
convert = fmapTree toInst 

-- auxial functions for RTL generator

isAdder :: Tree a -> Bool
isAdder (Node _ ts) | length ts == 3 = True
                    | otherwise      = False

carry :: Tree (Maybe (Inst a)) -> [Tree (Maybe (Inst a))]
carry t = foldrTree f [] t
  where f x = if isAdder x then (x{subForest = []}:) else id

giveNumber :: [b] -> Tree a -> Tree (b,a)
giveNumber = undefined
-- giveNumber (n:ns) t@(r ts) = Node (n,r) giveNumber 

tertiarize :: Tree (Maybe a) -> Tree (Maybe a)
tertiarize (Node r ts0) = (Node r (newForest ts0))
  where
    newForest [] = [] -- Leaf
    newForest [t] = [tertiarize t]
    newForest [t1,t2] = fmap tertiarize [t1,t2]
    newForest [t1,t2,t3] = fmap tertiarize [t1,t2,t3]
    newForest (t1:t2:t3:ts) = newForest ((tertiarize (Node Nothing [t1,t2,t3])):ts) 

wallace :: [Tree (Maybe (Inst a))] -> [Tree (Maybe (Inst a))]
wallace (x:[]) = [tertiarize x]
wallace (x:xs) = [tertiarize (Node Nothing (x:(carry (head (wallace xs)))))] ++ wallace xs

-- *****************************************************************************
--  configure
-- *****************************************************************************

nodePrefix :: String
nodePrefix = "N"

-- *****************************************************************************

nums = Just <$> [1..25]
numTree = foldr insertTree (singleton (Just 4)) nums

pins = Just <$> [1..4]
pinTree = foldr insertTree (singleton (Just 100)) pins

test0 = do 
  putStrLn . drawTree $ fmap (show . maybeToList) ((\t -> do fmap (\x -> if x == Nothing then Just 0 else x) t) (tertiarize pinTree))

test1 = do
  putStrLn . drawTree $ fmap (show . maybeToList) (tertiarize numTree)
test3 = do
  foldr (++) "" (fmap (\x -> show (maybeToList (rootLabel x))) (subForest (tertiarize numTree)))


