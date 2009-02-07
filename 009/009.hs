
combinations k n = combinationsOf k [1..n]
	where
	combinationsOf 0 _ = [[]]
	combinationsOf _ [] = []
	combinationsOf k (x:xs) = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs

bruteForce n = [ (a,b,c) | [a,b,c] <- combinations 3 n, (a^2) + (b^2) == (c^2), a+b+c == n ]


main = print(a*b*c)
	where [(a,b,c)] = take 1 (bruteForce 1000)
