
-- inefficient primes
--primes :: [Integer]
--primes = sieve [2..]
--         where sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]

-- need more efficient primes
--import Codec.Encryption.RSA.NumberTheory (primes)
import NumberTheory (primes)


sumOfPrimesBelow n = foldl (+) 0 (takeWhile (<n) primes)

main = print( sumOfPrimesBelow 2000000 )
