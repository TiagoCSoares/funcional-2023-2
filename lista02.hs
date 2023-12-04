--Nome: Tiago Costa Soares
--Matrícula: 2020.1.08.017

--Ex1
--Faça uma função que, dado um inteiro, retorne um booleano indicando se ele é perfeito (True) ou não 
--(False). Um número perfeito é aquele em que a soma de seus divisores (exceto ele mesmo) é igual ao 
--próprio número.

ehPerfeito:: Int -> Bool
ehPerfeito n 
    |n == (perfeito n (n-1)) = True
    |otherwise = False

perfeito:: Int -> Int -> Int 
perfeito _ 0 = 0
perfeito a b 
    |mod a b == 0 = b + perfeito a (b-1)
    |otherwise = perfeito a (b-1)

--ehPerfeito:: Int -> Bool 
--ehPerfeito n
--    |n == sum [x | x <- [1..(n-1)], mod n x == 0] = True 
--    |otherwise = False




--Ex2
--Faça uma função que receba como parâmetro uma String e retorne uma tupla com dois elementos. O 
--primeiro é um caractere qualquer da String e o outro é o número de vezes que ele apareceu a partir do 
--momento que ele foi descoberto. 
--Hugs > contaString “fellipe rey” 
--[(‘f’, 1), (‘e’, 3), (‘l’, 2), (‘l’, 1), (‘i’, 1), (‘p’, 1), (‘e’, 2), (‘ ’, 1), (‘r’, 1), (‘e’, 1), (‘y’, 1)] 
contaString:: String -> [(Char, Int)]
contaString [a] = [(a, 1)]
contaString (a:x) = (a, conta a x):(contaString x)


conta:: Char -> String -> Int
conta a [] = 1
conta a (b:x)
    |a == b = 1 + conta a x
    |otherwise = conta a x



--Ex 3
--Faça uma função que inverte uma String. 
--Hugs > inverte “Fellipe Rey” 
-- “yeR epilleF”


inverte:: String -> String 
inverte [] = []
inverte (a:x) = (inverte x)++[a]




--Ex 4
--Usando  generalização  (map,  filter  ou  foldr1),  faça  uma  função  que  calcule  o  quadrado  de  cada 
--elemento uma lista de entradas 
--Hugs > FUNÇÃO_DE_GENERALIZAÇÃO squares [1, -2, 3] 
--[1, 4, 9]
squares:: Int -> Int 
squares a = a*a

mapSquares:: (Int -> Int) -> [Int] -> [Int]
mapSquares f a = map f a



--Ex 5
--Faça uma função que simule um produto cartesiano entre duas listas de Int A e B e retorne uma lista 
--de tuplas Int, cada tupla com dois parâmetros. 
--Hugs > [1,2] [7,8] 
--[(1,7), (1,8), (2,7), (2,8), (7,1), (7,2), (8,1), (8,2)]
cartesiano:: [Int] -> [Int] -> [(Int, Int)]
cartesiano a b = (cartesiano2 a b)++(cartesiano2 b a)

cartesiano2:: [Int] -> [Int] -> [(Int, Int)]
cartesiano2 [] _ = []
cartesiano2 _ [] = []
cartesiano2 (a:x) (b:y) = ((montaCartesiano a (b:y))++(cartesiano2 x (b:y)))

montaCartesiano:: Int -> [Int] -> [(Int, Int)]
montaCartesiano a [b] = [(a,b)] 
montaCartesiano a (b:x) = (a,b):(montaCartesiano a x)



--Ex 6
--Usando  generalização  (map,  filter  ou  foldr1),  faça  uma  função  que  pegue  os  números  positivos  de 
--uma lista 
--Hugs > FUNÇÃO_DE_GENERALIZAÇÃO positives [1, -2, 3] 
--[1, 3]

filterPositive:: (Int -> Bool) -> [Int] -> [Int]
filterPositive f a = filter f a


positives:: Int -> Bool 
positives a
    | a > 0 = True
    | otherwise = False



--Ex 7
--Usando  generalização  (map,  filter  ou  foldr1),  faça  uma  função  que  some  o  dobro  de  todos  os 
--elementos de uma lista 
--Hugs > FUNÇÃO_DE_GENERALIZAÇÃO sumDouble [1, -2, 3] 
--4 
--Exemplo do cálculo: (1*2) + (-2 * 2) + (3 * 2) = 4
sumDouble:: Int -> Int
sumDouble a = a*2

mapDouble:: (Int -> Int) -> [Int] -> Int
mapDouble f a = sum (map f a)


--Ex 8
--Usando generalização (map, filter ou foldr1), faça uma função que concatene todas as Strings de uma 
--lista. 
--Hugs > FUNÇÃO_DE_GENERALIZAÇÃO concatena [“con”, “ca”, “te”, ‘na”] 
--concatena 
concatena:: String -> String -> String
concatena a b = a++b

foldr1Concatena:: (String -> String -> String) -> [String] -> String 
foldr1Concatena f a = foldr1 concatena a