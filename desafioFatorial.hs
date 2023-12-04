--Nome: Tiago Costa Soares
--Matrícula: 2020.1.08.017

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = sum [fatorial (n-1) | _ <- [1..n]]



fat:: Int -> Int -> Int
fat 0 _ = 1
fat 1 _ = 1
fat n n = 0 + fat(n n-1)
