import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Tree
import qualified TertiaryTree as T

-- data Inst a = Inst {cell :: String, name :: a}

nums = Just <$> [1..25]
numTree = T.foldr T.treeInsert (T.singleton (Just 4)) nums

test1 = do
  putStrLn . drawTree $ fmap (show . maybeToList) (T.tertiarize numTree)
test2 = do
  T.foldr (+) 0 (fmap (head . (1:) . maybeToList) numTree)
test3 = do
  foldr (++) "" (fmap (\x -> show (maybeToList (rootLabel x))) (subForest (T.tertiarize numTree)))


