--Nome: Tiago Costa Soares
--Matrícula: 2020.1.08.017


fatorial:: Int-> Int
fatorial 0 = 1
fatorial 1 = 1
fatorial n = somafatorial n (n-1)
	where
		somafatorial 1 n = fatorial n
		somafatorial a n = fatorial n + somafatorial (a-1) n
	
