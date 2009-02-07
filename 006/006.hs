
sumSquare n = foldl (+) 0 [ x*x | x <- [0..n] ]

squareSum n = x * x
	where x = foldl (+) 0 [ x | x <- [0..n] ]

diff n = 
	(squareSum n) - (sumSquare n)

main = print (diff 100)
