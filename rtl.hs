import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Tree
import TertiaryTree

data Inst = Inst {cell :: String, inst :: Maybe String, input :: [Maybe String]}
 deriving (Show)

-- |
-- >>> toInst "a"
-- Inst {cell = "", inst = Just "\"a\"", input = []}
toInst :: (Show a) => a -> Inst
toInst x = Inst "" (Just (show x)) []

isAdder :: Tree Inst -> Bool
isAdder (Node _ ts) | length ts == 3 = True
                    | otherwise      = False

tertiary :: Tree Inst       -> Tree Inst
tertiary (Node r ts) = Node r (newForest ts)
  where
    newForest [] = []
    newForest ts | length ts <= 3 = fmap tertiary ts
                 | otherwise      = let x = Node (Inst "sum_FA" Nothing [])
                                                 (take 3 ts)
                                    in newForest $ x:drop 3 ts

number   :: Int -> Tree Inst       -> Tree (Int,Inst)
number = numberTree

name     :: (Int, Inst) -> Inst
name (n, Inst c  Nothing  ins) = Inst c      (Just (show n)) ins
name (_, Inst "" (Just x) ins) = Inst "pin_" (Just x)        ins
name (n, Inst c  (Just x) ins) = Inst c      (Just x)        ins

step     :: Int -> Tree Inst       -> Tree Inst
step n t = fmap name . number n $ tertiary t

-- # 
carry    :: Tree Inst       -> [Tree Inst]
carry = foldrTree f []
  where f t' = let newTree = Node (rootLabel t'){cell = "co_FA"} [] 
               in if isAdder t' then (newTree:) else id

appendSubForest :: Tree Inst -> [Tree Inst] -> Tree Inst
appendSubForest t ts = Node (rootLabel t) (subForest t ++ ts)

wallace :: Int -> [Tree Inst] -> [Tree Inst]
wallace 0 ts = [step 0 (head ts)]
wallace n ts = step (n * 100) (appendSubForest (ts !! n) (carry (head lsbs))) : lsbs
  where
    lsbs = wallace (n-1) ts
--
-- *****************************************************************************
--  configure
-- *****************************************************************************

toString :: Inst -> String
toString i = head . maybeToList $ Just (cell i) `mappend` inst i

nodePrefix :: String
nodePrefix = "N"


main :: IO ()
main = do
  putStrLn . unlines . map (drawTree . fmap toString) . wallace 2 . replicate 3 . fmap toInst $ foldr insertTree (singleton 100) [1..10]
  putStrLn "done"
