import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Tree
import Data.Traversable
import RoseTree

type Name = Maybe String
data Inst = Inst {cell :: Name, inst :: Name}

instance Show Inst where
  show (Inst Nothing Nothing) = "X_unnamed"
  show (Inst c       i      ) = head . maybeToList $ mappend c i

addSubForest :: Tree a -> Tree a -> Tree a
addSubForest t (Node r ts) = Node r (ts ++ [t]) 

-- |
-- >>> tnes $ fmap (foldrTree (mappend . toNetline) []) toInst "a"
-- Inst {cell = Nothing, inst = Just "\"a\"", input = []}
toInst :: (Show a) => a -> Inst
toInst a = Inst Nothing (Just (show a)) 

-- |
-- >>> genMul 3
-- [["00"],["01","10"],["02","11","20"],["12","21"],["22"]]
genMul :: Int -> [Tree Inst]
genMul n = map (genTree . genCP n) $ reverse [0..(n * 2 - 1 - 1)]
  where
    genTree ::  [String] -> Tree Inst
    genTree xs = foldr addSubForest t0 (map (singleton . toInst) xs)
    t0 :: Tree Inst
    t0 = singleton (Inst Nothing Nothing)
    genCP :: Int -> Int -> [String]
    genCP n i = map (\(a,b) -> show a ++ show b) [(x,y) | x <- [0..n-1], y <- [0..n-1] , x + y == i]


isFA :: Tree Inst -> Bool
isFA (Node _ ts) | length ts == 3 = True
                 | otherwise      = False

isHA :: Tree Inst -> Bool
isHA (Node _ ts) | length ts == 2 = True
                 | otherwise      = False

isAdder :: Tree Inst -> Bool
isAdder x = isFA x || isHA x

tertiary :: Tree Inst -> Tree Inst
tertiary t@(Node r ts)
  | length ts  < 2 = t 
  | length ts == 2 = Node r{cell = Just "sum_HA"} (fmap tertiary ts)
  | length ts == 3 = Node r{cell = Just "sum_FA"} (fmap tertiary ts)
  | otherwise      = newForest t
  where
    newForest :: Tree Inst -> Tree Inst
    newForest (Node r ts)
      | length ts <= 3 = Node r ts -- error
      | otherwise      = let x = Node (Inst (Just "sum_FA") Nothing)
                                      (take 3 ts)
                         in tertiary (Node r (x:drop 3 ts))

number :: Int -> Tree Inst       -> Tree (Int,Inst)
number = numberTree

top_cell :: Tree Inst -> Tree Inst
top_cell (Node (Inst cell name) ts) = Node (Inst (Just "sum_FA") Nothing) ts

name :: (Int, Inst) -> Inst
name (n, Inst c       Nothing) = Inst c             (Just (show n))
name (_, Inst Nothing i      ) = Inst (Just "pin_") i
name (n, Inst c       i      ) = Inst c             i

step :: Int -> Tree Inst       -> Tree Inst
step n t = fmap name . number n $ tertiary t

-- # 
carry :: Tree Inst -> [Tree Inst]
carry = foldrTree f []
  where f t' = let newTreeFA = Node (rootLabel t'){cell = Just "co_FA"} [] 
                   newTreeHA = Node (rootLabel t'){cell = Just "co_HA"} []
               in if isFA t' then (newTreeFA:)
                  else if isHA t' then (newTreeHA:)
                  else id

appendSubForest :: Tree Inst -> [Tree Inst] -> Tree Inst
appendSubForest t ts = Node (rootLabel t) (subForest t ++ ts)

wallace :: [Tree Inst] -> [Tree Inst]
wallace = wallace_core
  where 
    wallace_core :: [Tree Inst] -> [Tree Inst]
    wallace_core (t:[]) = [step 0 t]
    wallace_core (t:ts) = step (n * 100) (appendSubForest t (carry (head lsbs)))
                     : lsbs
      where
        lsbs = wallace_core ts
        n    = length ts + 1 

toNetline :: Tree Inst -> String
toNetline (Node r ts) | cell r == Just "sum_FA" = unwords xsFA
                      | cell r == Just "sum_HA" = unwords xsHA
                      | otherwise               = ""
  where xsFA = "FA_CELL_NAME" : co : so : map (show . rootLabel) ts ++ ["\n"]
        xsHA = "HA_CELL_NAME" : co : so : map (show . rootLabel) ts ++ ["\n"]
        i    = head . maybeToList $ inst r
        co   = "co_" ++ i
        so   = "sum_" ++ i

main :: IO ()
main = do
  -- result <- return . wallace 2 . replicate 3 . fmap toInst $ foldr insertTree (singleton 100) [1..10]
  -- putStrLn . unlines $ map (foldrTree (mappend . toNetline) []) result 
  result <- return . wallace $ genMul 6 
  putStrLn . unlines $ map (foldrTree (mappend . toNetline) []) result
  putStrLn . unlines $ (map showTree) result  
  putStrLn "done"

