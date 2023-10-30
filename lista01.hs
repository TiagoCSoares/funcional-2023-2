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
ordenaEmTupla a b c d = (menor a (menor b (menor c d)), min a (max b ( min c d)) , min a (max b (max c d)), max a (max b (max c d)))

menor :: Int -> Int -> Int
menor a b
    | a > b = b 
    | otherwise = a 


-- organizaLista :: [Int] -> (Int, Int, Int, Int)
-- organizaLista [] = 0
-- organizaLista a:x = max a (organizaLista x)




--Defina a função quantosDias que, dado um ano, retorna o número de dias do ano.
--Hugs > quantosDias 2002
--365
--Hugs > quantosDias 2000
--366
quantosDias :: Int -> Int
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
maiormenor :: [Int] -> (Int,Int)
maiormenor [a] = (a, a)
maiormenor (a:x) = (menorl [a] (menorl x), maiorl a (maiorl x))

maiorl :: [Int] -> [Int] -> Int
maiorl (a:x) (b:y)
    | a > b = a 
    | otherwise = b

menorl :: [Int] -> [Int] -> Int
menorl (a:x) (b:y)
    | a > b = a 
    | otherwise = b
