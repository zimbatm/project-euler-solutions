
divs [] = []
divs (x:xs) =
	if foldl (\sum y -> (y `mod` x == 0) || sum) False xs
	then divs xs
	else (x:divs xs)

divby1to n = verify 2
	where 
		lst = divs [1..n]
		verify n =
			if verif2 n lst then n
			else verify (n+1)
		verif2 n [] = True
		verif2 n (x:xs) = 
			if n `mod` x == 0
			then verif2 n xs
			else False
			

main = print (divby1to 20)
