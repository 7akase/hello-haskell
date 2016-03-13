import Data.Bits

countToggleCunit :: [Int] -> Int
countToggleCunit xs = sum $ zipWith xor xs (0 : xs)


signalToCdacPattern :: [Float] -> [Int]
signalToCdacPattern xs = map ((+128).truncate.(/vstep)) xs
                           where vstep = 0.01

-- sample
-- countToggleCunit . signalToCdacPattern $ map (sin.(*(1.6/2))) [0..2^4]
