import List (sort)

-- very inefficient

isPalindrome num = (reverse (show num) == (show num))

palOf n max = [ x * n | x <- [n..max], isPalindrome (x*n) ]

palindromes min max = [ palOf x max | x <- [min..max] ] 

tmp = foldl (++) [] (palindromes 100 999)

tmp2 = sort tmp

main = print (head (reverse tmp2))

