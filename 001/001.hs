

mul3and5 :: [Int]
mul3and5 = [ x | x <- [0 .. 999], (mod x 5 == 0) || (mod x 3 == 0) ] 

result = foldl (+) 0 mul3and5

main = print result
