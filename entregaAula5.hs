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

digits :: [Char] -> [Char]
digits a = filter isDigit a


ex4 :: [Char] -> [Char]
ex4 [] = []
ex4 (a:x)
    |isDigit a = ex4 x
    |otherwise = a:(ex4 x)
    
    

letters :: [Char] -> [Char]
letters [] = []
letters (a:x)
    |isDigit a = letters x
    |otherwise = a:(letters x)
