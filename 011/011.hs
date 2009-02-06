{-# LANGUAGE TemplateHaskell #-}
-- Cool haskell template given by `thetallgu1` on #haskell that imports the
-- data at parse-time
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (lift)
dataTable :: String
dataTable = $(runIO (readFile "table.txt") >>= lift)

-- Transforms file data into an array of array of ints
readMatrix :: String -> [[Integer]]
readMatrix str = map (\line -> map (\num -> read (num) :: Integer) (words line)) (lines str)

-- 
rotate90 mat = [[mat !! y !! x | y <- [0..height-1] ] | x <- reverse [0..width-1] ]
  where
    width = length (mat!!0)
    height = length mat

test_rotate90 =
  [[2,4],[1,3]] == rotate90 [[1,2],[3,4]] &&
  [[3,6,9],[2,5,8],[1,4,7]] == rotate90 [[1,2,3],[4,5,6],[7,8,9]]

-- 
rowMul :: [Integer] -> Int -> [Integer]
rowMul xs len
  | length xs < len = []
  | otherwise = (product (take len xs)) : rowMul (tail xs) len

test_rowMul = 
  [2,6,12] == (rowMul [1..4] 2) &&
  [2,6,12,20] == (rowMul [1..5] 2)

-- 
matMul :: [[Integer]] -> Int -> [[Integer]]
matMul [] _ = []
matMul (x:xs) len = (rowMul x len) : (matMul xs len)

test_matMul =
  [[2],[12]] == (matMul [[1,2],[3,4]] 2)

-- 
matMulDiag :: [[Integer]] -> Int -> [[Integer]]
matMulDiag mat len = [[(multDiag x y len) | x <- [0..(length (mat !! y)) - len]] | y <- [0..(length mat) - len]]
  where
    multDiag x y 0 = 1
    multDiag x y len = (mat !! (y+l1) !! (x+l1)) * multDiag x y l1
      where l1=len-1
      
test_matMulDiag =
  [[4]] == (matMulDiag [[1,2],[3,4]] 2) &&
  [[4,10],[18,28]] == (matMulDiag [[1,2,3],[3,4,5],[5,6,7]] 2)

--
matMax :: [[Integer]] -> Integer
matMax mat = foldl (\sum l -> (foldl max sum l) ) 0 mat

--
allMult mat len = [
  (matMul mat len),
  (matMul (rotate90 mat) len),
  (matMulDiag mat len),
  (matMulDiag (rotate90 mat) len)
  ]
  
allMax mat len = map matMax (allMult mat len)

findBiggest mat len = foldl max 0 (allMax mat len)
--
--
test_all = test_rotate90 && test_rowMul && test_matMul && test_matMulDiag

dataMatrix = readMatrix dataTable

main = print (findBiggest dataMatrix 4)

