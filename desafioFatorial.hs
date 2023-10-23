--Nome: Tiago Costa Soares
--Matrícula: 2020.1.08.017

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = sum [fatorial (n-1) | _ <- [1..n]]
