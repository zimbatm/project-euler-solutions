
primes :: [Integer]
primes = 2:filter isPrime [3,5..]
	where
	isPrime n   = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes
	divides n p = n `mod` p == 0


-- off by one :
main = print (primes !! 10000)
