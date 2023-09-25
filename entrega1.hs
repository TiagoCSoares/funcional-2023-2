-- Nome: Tiago Costa Soares
-- Matrícula: 2020.1.08.017

-- Ex1 
-- Implementação de uma função que calcula a distância euclidiana

euclidiana :: Float -> Float -> Float -> Float -> Float
euclidiana x1 y1 x2 y2
	|x1 == x2 = y2 - y1
	|y1 == y2 = x2 - x1
	|otherwise = sqrt (quadrado(x2 - x1) + quadrado(y2 - y1))
	
quadrado :: Float -> Float
quadrado x = x*x

-- Ex2
-- Faça uma função que determine a quantidade de raízes reais 
-- de uma equação de segundo grau

raizes :: Float -> Float -> Float -> Float
raizes a b c
	|(delta a b c) < 0 = 0
	|(delta a b c) == 0 = 1
	|otherwise = 2
	
	
delta :: Float -> Float -> Float -> Float
delta a b c = b*b - 4*a*c 
