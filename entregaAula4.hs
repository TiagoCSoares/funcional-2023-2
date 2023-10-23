-- Nome: Tiago Costa Soares
-- Matrícula: 2020.1.08.017


-- Faça uma função que retorne os n primeiros termos de uma lista,
-- sabendo que o número de elementos de uma lista vazia é zero.

retornaNElementos:: [x] -> Int -> [x]
retornaNElementos [] b = []
retornaNElementos _ 0 = []
retornaNElementos (a:x) y = a:(retornaNElementos x (y-1))




-- Faça uma função para inverter os elementos de uma lista.

inverteLista:: [a] -> [a]
inverteLista [] = []
inverteLista (a:x) = ((inverteLista x)++[a])



-- Construa uma função zip que agrupe duas listas, elemento a elemento,
-- combinando-os em pares (tupla-2), gerando uma única lista ao final.

agrupaListas:: [a] -> [b] -> [(a,b)]
agrupaListas _ [] = []
agrupaListas [] _ = []
agrupaListas (x:y) (c:d) = (x,c):(agrupaListas y d)



-- Faça uma função que retorne o maior valor numérico de uma lista.
maiorValor:: [Int] -> Int
maiorValor [] = 0
maiorValor (a:x) = max a (maiorValor x)




-- Faça uma função que adicione um objeto à uma lista sem repetições,
-- ou seja, caso o objeto já exista na lista, o mesmo não deve ser
-- adicionado.

adicionaSemRepeticao:: Eq a =>  [a] -> a -> [a]
adicionaSemRepeticao [] x = [x]
adicionaSemRepeticao a b
    | verifica a b = a++[b]
    | otherwise = a


verifica:: Eq a => [a] -> a -> Bool
verifica [] _ = True
verifica (x:y) z
    | x == z = False
    | otherwise = verifica y z
