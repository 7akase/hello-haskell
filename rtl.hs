import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Tree
import Data.Traversable
import TertiaryTree

type Name = Maybe String
data Inst = Inst {cell :: Name, inst :: Name}

instance Show Inst where
  show = head . maybeToList . (mappend cell inst)

name_to_str :: Name -> String
name_to_str Nothing  = ""
name_to_str (Just x) = x


-- |
-- >>> tnes $ fmap (foldrTree (mappend . to_netline) []) toInst "a"
-- Inst {cell = Nothing, inst = Just "\"a\"", input = []}
to_inst :: (Show a) => a -> Inst
to_inst a = Inst Nothing (Just (show a)) 

-- |
-- >>> genCarryPropagate 3
-- [["00"],["01","10"],["02","11","20"],["12","21"],["22"]]
genCarryPropagate :: Int -> [Inst]
genCarryPropagate n = fmap to_inst $ map (genCP n) [0..(n * 2 - 1 - 1)]
  where
    genCP n i = map (\(a,b) -> show a ++ show b) [(x,y) | x <- [0..n-1], y <- [0..n-1] , x + y == i]


isAdder :: Tree Inst -> Bool
isAdder (Node _ ts) | length ts == 3 = True
                    | otherwise      = False

tertiary :: Tree Inst -> Tree Inst
tertiary (Node r ts) = Node r (newForest ts)
  where
    newForest [] = []
    newForest ts | length ts <= 3 = fmap tertiary ts
                 | otherwise      = let x = Node (Inst (Just "sum_FA") Nothing)
                                                 (take 3 ts)
                                    in newForest $ x:drop 3 ts

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
  where f t' = let newTree = Node (rootLabel t'){cell = Just "co_FA"} [] 
               in if isAdder t' then (newTree:) else id

appendSubForest :: Tree Inst -> [Tree Inst] -> Tree Inst
appendSubForest t ts = Node (rootLabel t) (subForest t ++ ts)

wallace :: Int -> [Tree Inst] -> [Tree Inst]
wallace 0 ts = [step 0 (head ts)]
wallace n ts = step (n * 100) (appendSubForest (ts !! n) (carry (head lsbs))) : lsbs
  where
    lsbs = wallace (n-1) ts

-- *****************************************************************************
--  configure
-- *****************************************************************************
--------------------------------------------------------------------------  
to_netline :: Tree Inst -> String
to_netline (Node r ts) | cell r == Just "sum_FA" = unwords xs
                       | otherwise               = ""
  where xs = "FA_CELL_NAME" : co : so : map (show . rootLabel) ts ++ ["\n"]
        i  = head . maybeToList $ inst r
        co = "co_" ++ i
        so = "sum_" ++ i
--------------------------------------------------------------------------  
t = wallace 2 . replicate 3 . top_cell . fmap to_inst $ foldr insertTree (singleton 100) [1..10]

main :: IO ()
main = do
  result <- return . wallace 2 . replicate 3 . fmap to_inst $ foldr insertTree (singleton 100) [1..10]
  putStrLn . unlines $ fmap (foldrTree (mappend . to_netline) []) t
  putStrLn "done"

