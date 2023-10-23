--Nome: Tiago Costa Soares
--Matrícula: 2020.1.08.017

import Data.Char

ex1 :: [t] -> [t]
ex1 [] = []
ex1 (a:x) = (ex1 x) ++ [a]


ex2 :: [t] -> [t] -> [(t,t)]
ex2 _ [] = []
ex2 [] _ = []
ex2 (a:x) (b:y) = (a,b):(ex2 x y)


duplastuplas :: [t] -> [t] -> [(t,t)]
duplastuplas _ [] = []
duplastuplas [] _ = []
duplastuplas (a:x) (b:y) = (a,b):(duplastuplas x y)


ex3 :: [Char] -> [Char]
ex3 a = filter isDigit a


teste:: [Int] -> Int
teste [] = 0
teste (a:x) = a

ex4 :: [Char] -> [Char]
ex4 [] = []
ex4 (a:x)
    |isDigit a = ex4 x
    |otherwise = a:(ex4 x)