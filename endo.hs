import Data.Monoid
import Data.Tree
import Data.Foldable

main = do 
 tree <- return $  unfoldTree (\x -> (x, [1..x-1])) 4
 putStrLn . show $ appEndo (foldMap (\x -> if even x then Endo (x *) else Endo (+ x)) tree) 0
 
 -- The following line has error because both (x *) and (+ x) are not Monoid
 -- putStrLn . show $ foldMap (\x -> if even x then (x *) else (+ x)) tree 0
