import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs ->  ((), (a:xs))

stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop

stackManip2 :: State Stack ()
stackManip2 = do
  push 3
  a <- pop
  pop
  push 3
