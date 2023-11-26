--Nome: Tiago Costa Soares
--Matrícula: 2020.1.08.017

isEven:: Int -> Bool
isEven a 
    | mod a 2 == 0 = True
    | otherwise = False 





maxi2:: Int -> Int -> Int 
maxi2 a b 
    | a >= b = a 
    | otherwise = b 



nonEmpty:: [Char] -> Bool
nonEmpty [] = False
nonEmpty (a:x) = True