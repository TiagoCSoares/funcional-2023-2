ex1:: [Int] -> Int -> [Int]
ex1 [] _ = []
ex1 _ 0 = []
ex1 (a:x) n
    | n > 0 = a:(ex1 x (n-1))
    | otherwise = []

ex2:: [Int] -> Int -> Bool
ex2 [] _ = False
ex2 (a:x) n 
    | a == n = True 
    | otherwise = ex2 x n

ex3:: [Int] -> Int
ex3 [a] = a 
ex3 (a:x) = max a (ex3 x)

ex4:: [Int] -> [Int]
ex4 [] = []
ex4 (a:x) = (ex4 x)++[a]

ex5:: [Int] -> Int
ex5 [a] = a 
ex5 (a:x) = ex5 x

ex6:: [Int] -> Int -> Int
ex6 (a:x) n 
    | n == 1 = a 
    | otherwise = ex6 x (n-1)