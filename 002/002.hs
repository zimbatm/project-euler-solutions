
fibs :: [Int]
fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs)]

pairFibs = [ x | x <- fibs, mod x 2 == 0 ]

pairFibsBelow4mio = blablaGen [] pairFibs

blablaGen lst (x:xs) = if x < 4000000
	then blablaGen (x:lst) xs
	else lst

result = foldr (+) 0 pairFibsBelow4mio

main = print result
