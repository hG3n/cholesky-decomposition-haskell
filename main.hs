-- create a new matrix with equal dimension
createEmptyMatrix :: Int -> [Float]
createEmptyMatrix dim = take (dim*dim) (repeat 0.0)

-- get element from matrix via row, column notation
get :: (RealFrac p, Floating p) => p -> p -> [a] -> a
get row col mat = mat !! (round (row * (sqrt (fromIntegral (length mat))) + col))

-- alters the element with matching row, col coordinates
-- assuming the given r,c values are within matrix range
alterListElement :: (RealFrac p, Floating p) => p -> p -> [a] -> a -> [a]
alterListElement r c list new_element = 
  take element_idx list ++ [new_element] ++ drop (element_idx + 1) list
  where element_idx = round (r * (sqrt (fromIntegral (length list))) + c)

-- calculate the indices of a the lower left triangle
-- returning a list of lists, each containing row and col values
-- e.g [ [0,0], [0,1], [1,0], ... ]
calcLowerTriangleIndices :: (Num p, Enum p) => p -> [[p]]
calcLowerTriangleIndices dimension = 
  concat [ [ [x,y] | y <- [0..x] ] | x <- [0..dimension-1] ]

-- cholesky helper for processing diagonal matrix values with equal indices
choleskyHelperDiag :: (Enum p, RealFrac p, Floating a, Floating p) => p -> p -> [a] -> [a] -> [a]
choleskyHelperDiag i j matrix result = alterListElement i j result value
  where value = sqrt( (get i i matrix) - s)
        s = sum [ (get i k result) * (get j k result) | k <- [0..j] ]

-- cholesky helper for processing the lower matrix result
-- without considering diagonal values
choleskyHelperLower :: (Fractional a, Enum p, RealFrac p, Floating p) => p -> p -> [a] -> [a] -> [a]
choleskyHelperLower i j matrix result = alterListElement i j result value
  where value = (1.0 / (get j j result) * (get i j matrix - s))
        s = sum [ (get i k result) * (get j k result) | k <- [0..j] ]

-- cholesky helper for checking current matrix indices
-- infering resulting action 
choleskyIndexCheck :: (Floating p, Floating a, RealFrac p, Enum p) => p -> p -> [a] -> [a] -> [a]
choleskyIndexCheck i j result matrix
    | i == j = choleskyHelperDiag i j matrix result
    | otherwise = choleskyHelperLower i j matrix result

-- recursive cholesky decomposition
-- assuming the given matrix has equal dimensions
cholesky:: (Enum p, RealFrac p, Floating a, Floating p) => [a] -> [a] -> [[p]] -> Int -> [a]
cholesky matrix result indices start
    | start <= ((length indices)-1) = cholesky matrix (choleskyIndexCheck i j result matrix) indices (succ start) 
    | otherwise = result 
      where i = indices !! start !! 0
            j = indices !! start !! 1

-- main
-- decompose two different matrices
-- a matrix is described by a simple list with n^2 elements
main :: IO()
main =  do
  -- example 1 
  let matrix_1 = [25, 15, -5, 15, 18, 0, -5, 0, 11]
  let result = createEmptyMatrix 3
  let indices = calcLowerTriangleIndices 3
  let res = cholesky matrix_1 result indices 0
  putStrLn "decomposing matrix 1"
  print  matrix_1
  putStrLn "to"
  print res
  putStrLn ""
  putStrLn ""

  -- example 2
  let matrix_2 = [18, 22,  54,  42, 22, 70,  86,  62, 54, 86, 174, 134, 42, 62, 134, 106]
  let result_2 = createEmptyMatrix 4
  let indices_2 = calcLowerTriangleIndices 4
  let res_2 = cholesky matrix_2 result indices 0
  putStrLn "decomposing matrix 2"
  print  matrix_2
  putStrLn "to"
  print res_2

