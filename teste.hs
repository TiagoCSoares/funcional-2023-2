--Ex 1
--Faça uma função que verifique se um número inteiro de entrada é primo. 
--Lembrando: O número primo é divisível apenas por 1 e por ele mesmo. 
--O número 1 (um) não é primo e o número 2 (dois) é o único número par que é primo. 
--Hugs > ehPrimo 37 
--True.


ehPrimo:: Int -> Bool
ehPrimo a 
    | a < 2 = False
    | a == 2 = True
    | otherwise = verifica a 2 


verifica:: Int -> Int -> Bool
verifica a b
    | a == b = True
    | mod a b == 0 = False
    | otherwise = verifica a (b+1)




--Ex 2
--Construa uma função em Haskell que recebe quatro inteiros e devolva uma tupla-4 com os 
--quatro valores originais, mas ordenados 
--Hugs> ordenaEmTupla 7 0 5 3 
--(0, 3, 5, 7)

ordenaEmTupla:: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
ordenaEmTupla a b c d =  tupla(ordena[a,b,c,d])

ordena:: [Int] -> [Int]
ordena [] = []
ordena (cabeca:cauda) =  ordena[x | x <- cauda, x < cabeca]++[cabeca]++ordena[x | x <- cauda, x >= cabeca]

tupla:: [Int] -> (Int, Int, Int, Int)
tupla [a,b,c,d] = (a,b,c,d)



--Ex 3
--Defina a função quantosDias que, dado um ano, retorna o número de dias do ano.
--Hugs > quantosDias 2002
--365
--Hugs > quantosDias 2000
--366

quantosDias:: Int -> Int
quantosDias a
    | mod a 4 == 0 = 366
    | otherwise = 365 



-- Ex 4
--Defina a função diasMes que, dados um ano e um mês, devolve o número de dias desse mês nesse
--ano.
--Hugs > diasMes 2004 1
--31
--Hugs > diasMes 2004 2
--29
--Hugs > diasMes 2005 2
--28

diasMes:: Int -> Int -> Int
diasMes a 2 
    | quantosDias a == 366 = 29
    | otherwise = 28
diasMes _ b 
    | b == 1 ||  b == 3 || b == 5 || b == 7 || b == 8 || b == 10 || b == 12 = 31
    | otherwise = 30





--Ex 5
--Defina a função dia que, dados um ano, um mês e um dia do mês, devolve o número de ordem desse 
--dia nesse ano. Se os dados não constituírem uma data válida, a função deve devolver -1. 
--Hugs > dia 2004 1 1 
--1 
--Hugs > dia 2003 1 31 
--31 
--Hugs > dia 2004 2 1 
--32 
--Hugs > dia 2004 12 31 
--366 
--Hugs > dia 2005 12 31 
--365 
--Hugs > dia 2005 2 29 
---1


--dia:: Int -> Int -> Int -> Int
--dia a 2 c
--    | quantosDias a == 365 && c >= 29 = -1
--    | quantosDias a == 366 && c > 30 = -1
--    | otherwise = 31+c
--dia a b c 
--    | quantosDias a == 365 && b == 1  


dat :: Int -> Int -> Int -> Int
dat ano mes dia
    | not (dataValida ano mes dia) = -1
    | otherwise = somaDias ano mes dia

dataValida :: Int -> Int -> Int -> Bool
dataValida ano mes dia
    | mes < 1  || mes > 12  || dia < 1 = False
    | mes == 2 = dia <= diasMes ano mes
    | otherwise = dia <= diasMes ano mes

somaDias :: Int -> Int -> Int -> Int
somaDias ano mes dia = sum [diasMes ano m | m <- [1..mes-1]] + dia





-- Ex 6
-- Sem utilizar as funções max e min, faça uma função que receba uma lista e devolva uma dupla
-- contendo o menor e o maior elemento
-- Hugs > maioremenor [2,5,1,6,9,3,0,4]

maiormenor:: [Int] -> (Int, Int)
maiormenor 

