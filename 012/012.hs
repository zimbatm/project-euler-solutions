primes :: [Integer]
primes = 2:filter isPrime [3,5..]
  where
    isPrime n   = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes
    divides n p = n `mod` p == 0

-- Using serie convergence for better perfs than: sum [1..n] for each n
n1serie :: [Integer]
n1serie = map (\n -> div (n * (n+1)) 2) [1..]

primeDiv p n times = if mod n (p*times) == 0 then primeDiv p n (times+1)
  else times
--numDivisors n = 1+length (filter (\x -> mod n x== 0) [2..ceiling (sqrt n)])
numDivisors n = product [primeDiv p n 1 | p <- takeWhile (< n-1) primes ]

--numDivisors n = product [b | (_,b) <- primePowerFactors n ]

maxDivisors n xs = getFirst (\x -> numDivisors x > n) xs
	where
		getFirst fn (x:xs) = if (fn x) then x
			else getFirst fn xs 

main = print (maxDivisors 500 n1serie)
