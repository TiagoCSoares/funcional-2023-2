type Meu_tipo = (String, Float, Char)

pessoa :: Float -> Meu_tipo
pessoa rg
	| rg == 1 = ("Joao Silva",12,'m')
	| rg == 2 = ("Jonas Souza",81,'m')
	| rg == 3 = ("Joice Silva",12,'f')
	| rg == 4 = ("Janete Souza",10,'f')
	| rg == 5 = ("Jocileide Strauss",21,'f')
	| otherwise = ("Nao há mais ninguem",0,'x')

base :: Int -> (Int, String, String, Char)
base x
	|x == 0 = (1793, "Pedro Paulo", "MESTRE",'M')
	|x == 1 = (1797, "Joana S. Alencar", "MESTRE",'M')
	|x == 2 = (1534, "Joao de Medeiros", "DOUTOR",'F')
	|x == 3 = (1267, "Claudio Cesar de Sá", "DOUTOR",'M')
	|x == 4 = (1737, "Paula de Medeiros", "MESTRE",'F')
	|x == 5 = (1888, "Rita de Matos", "MESTRE",'F')
	|x == 6 = (1356, "Rodolfo Roberto", "DOUTOR", 'M')
	|x == 7 = (1586, "Célia Maria de Sousa", "DOUTOR", 'F')
	|x == 8 = (1800, "Josimar Justino", "MESTRE", 'M')
	|x == 9 = (1698, "Tereza C. Andrade", "MESTRE",'F')
	|x == 10 = ( 0, "" , "" ,'0')



