--Ex 1
--Faça uma função que verifique se um número inteiro de entrada é primo. 
--Lembrando: O número primo é divisível apenas por 1 e por ele mesmo. 
--O número 1 (um) não é primo e o número 2 (dois) é o único número par que é primo. 
--Hugs > ehPrimo 37 
--True.

ehPrimo :: Int -> Bool
ehPrimo n 
    | n < 2 = False
    | n == 2 = True
    | otherwise = verifica n 2


verifica :: Int -> Int -> Bool
verifica a b 
    | b == a = True
    | mod a b == 0 = False
    | otherwise = verifica a (b+1)





--Ex 2
--Construa uma função em Haskell que recebe quatro inteiros e devolva uma tupla-4 com os 
--quatro valores originais, mas ordenados 
--Hugs> ordenaEmTupla 7 0 5 3 
--(0, 3, 5, 7)

ordenaEmTupla :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
ordenaEmTupla a b c d = tupla(qsort[a,b,c,d])

qsort :: [Int] -> [Int]
qsort [] = []
qsort (head:tale) = qsort [y | y <- tale, y < head]++[head]++qsort [y | y <- tale, y >= head]

tupla :: [Int] -> (Int, Int, Int, Int)
tupla [a,b,c,d] = (a,b,c,d) 




--Ex 3
--Defina a função quantosDias que, dado um ano, retorna o número de dias do ano.
--Hugs > quantosDias 2002
--365
--Hugs > quantosDias 2000
--366
quantosDias :: Int -> Int
quantosDias a
    | mod a 100 == 0 && mod a 400 /= 0 = 365
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

diasMes :: Int -> Int -> Int
diasMes a 2 
    | quantosDias a == 366 = 29
    | otherwise = 28
diasMes _ b 
    | b == 1 ||  b == 3 || b == 5 || b == 7 || b == 8 || b == 10 || b == 12 = 31
    | otherwise = 30






-- Ex 6
-- Sem utilizar as funções max e min, faça uma função que receba uma lista e devolva uma dupla
-- contendo o menor e o maior elemento
-- Hugs > maioremenor [2,5,1,6,9,3,0,4]
maioremenor :: [Int] -> (Int,Int)
maioremenor (a:[]) = (a, a)
maioremenor a = (menorLista a, maiorLista a)



menorLista :: [Int] -> Int
menorLista [a] = a
menorLista (a:x) = menor a (menorLista x)


menor :: Int -> Int -> Int
menor a b 
    | a > b = b    
    | otherwise = a

maiorLista :: [Int] -> Int
maiorLista [a] = a
maiorLista (a:x) = maior a (maiorLista x)

maior :: Int -> Int -> Int
maior a b 
    | a > b = a    
    | otherwise = b



--Ex 7
--Faça uma função (ou mais) que recebe uma lista com números e retorna outra lista com os números
--ordenados:
--Hugs > ordena [7, 3, 5, 7, 8, 4, 4]
--[3, 4, 4, 5, 7, 7, 8]


ordena :: [Int] -> [Int]
ordena [] = []
ordena (a:b) = ordena [x | x <- b,  x < a] ++ [a] ++ ordena [x | x <- b, x >= a]


--Ex 8
--Faça uma função que, dada uma lista de inteiros, retorna uma lista com repetição de cada elemento de
--acordo com seu valor.
--Hugs > repeteElemento [1,2,3,4,5]
--[1,2,2,3,3,3,4,4,4,4,5,5,5,5,5]

repeteElemento :: [Int] -> [Int]
repeteElemento [] = []
repeteElemento (a:b) = (repete a a)++(repeteElemento b)

repete :: Int -> Int -> [Int]
repete 0 _ = []
repete _ 0 = []
repete a b = a:(repete a (b-1))


--Ex 9
--Faça uma função que calcula a série:
-- serie = (1/x)+(x/2)+(3/x)+(x/4)
--Você deve passar dois números por parâmetro: o primeiro contendo o valor de x e o segundo o
--número de elementos da série (Para facilitar, use somente o valor inteiro – ignore as casas decimais).
--Main> serie 1 100
--2500
--Main> serie 2 100
--1226

serie :: Int -> Int -> Int
serie _ 0 = 0
serie a 1 = div 1 a
serie a b
    | mod b 2 == 0 = div a b + (serie a (b-1))
    | otherwise = div b a + (serie a (b-1)) 