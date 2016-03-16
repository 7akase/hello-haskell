-- data FA a = In a | Co (a,a,a) | S (a,a,a)
data Adder a = FA a a a | HA a a | In a deriving (Show)

-- bitToFA :: [a] -> [[a]]
-- bitToFA [] = []
-- bitToFA (n1:n2:n3:ns) = [[n1,n2,n3]] ++ bitToFA ns
-- bitToFA ns = [ns]

bitToFA :: [a] -> [Adder a]
bitToFA [] = []
bitToFA [n1] = [In n1]
bitToFA [n1,n2] = [HA n1 n2]
bitToFA (n1:n2:n3:ns) = [FA n1 n2 n3] ++ bitToFA ns

