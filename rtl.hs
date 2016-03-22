import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Tree
import qualified TertiaryTree as T

data Inst a = Inst {cell :: String, name :: Maybe a, from :: [Maybe a]}
 deriving (Show)

toInst :: Tree (Maybe a) -> Tree (Maybe (Inst a))
toInst (Node Nothing  ts) = Node (Just (Inst "" Nothing 
                                             (fmap (\(Node r _) -> r) ts)))
                                 (fmap (T.fmapT toInst) ts)
toInst (Node (Just x) ts) = Node (Just (Inst "" (Just x)
                                             (fmap (\(Node r _) -> r) ts)))
                                 (fmap (T.fmapT toInst) ts)
 
-- usage : putStrLn . T.showT . convert $ T.tertiarize numTree
convert :: Tree (Maybe a) -> Tree (Maybe (Inst a))
convert = T.fmapT toInst 

-- auxial functions for RTL generator

isAdder :: Tree a -> Bool
isAdder (Node _ ts) | length ts == 3 = True
                    | otherwise      = False

carry :: Tree (Maybe (Inst a)) -> [Tree (Maybe (Inst a))]
carry t = T.foldrT f [] t
  where f x = if isAdder x then (x{subForest = []}:) else id


wallace :: [Tree (Maybe (Inst a))] -> [Tree (Maybe (Inst a))]
wallace (x:[]) = [T.tertiarize x]
wallace (x:xs) = [T.tertiarize (Node Nothing (x:(carry (head (wallace xs)))))] ++ wallace xs

-- *****************************************************************************
--  configure
-- *****************************************************************************

nodePrefix :: String
nodePrefix = "N"

-- *****************************************************************************

nums = Just <$> [1..25]
numTree = T.foldr T.treeInsert (T.singleton (Just 4)) nums

pins = Just <$> [1..4]
pinTree = T.foldr T.treeInsert (T.singleton (Just 100)) pins

test0 = do 
  putStrLn . drawTree $ fmap (show . maybeToList) ((\t -> do fmap (\x -> if x == Nothing then Just 0 else x) t) (T.tertiarize pinTree))

test1 = do
  putStrLn . drawTree $ fmap (show . maybeToList) (T.tertiarize numTree)
test2 = do
  T.foldr (+) 0 (fmap (head . (1:) . maybeToList) numTree)
test3 = do
  foldr (++) "" (fmap (\x -> show (maybeToList (rootLabel x))) (subForest (T.tertiarize numTree)))


