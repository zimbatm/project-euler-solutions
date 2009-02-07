
-- inefficient primes
primes :: [Integer]
primes = sieve [2..]
	where sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]

divides p n = (mod n p == 0)

primesOf n = gen [] primes n where 
	gen res (x:xs) n =
		if x > n then res
		else if x `divides` n then gen (x:res) xs (n `div` x)
		else gen res xs n

main = print (primesOf 600851475143)

