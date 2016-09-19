--2a
esPar :: Int -> Bool
esPar x = mod x 2 == 0

eXp :: Int -> Int -> Int
eXp x 0 = 1
eXp x n = x * eXp x (n-1)

iga :: Eq a => a -> [a] -> Bool
iga e [] =True
iga e (x:xs) = (x==e) && iga e xs 

--busca :: (Ord a, Eq a, Num a, Bounded a)
--			=> a -> [a] -> a
--busca e [] = maxBound
--busca e (x:xs) | x==e = min 0 (1 + busca e xs)
--			     | x/=e = (1 + busca e xs)

cuantos :: (a -> Bool) -> [a] -> Int
cuantos p [] = 0
cuantos p (x:xs) | p x == False = cuantos p xs
				 | otherwise = 1 + cuantos p xs 

--3
data Titulo = Ducado | Marquesado | Condado | Vizcondado | Baronia
hombre :: Titulo -> String
hombre Ducado = "Duque"
hombre Marquesado = "Marques"
hombre Condado = "Condado"
hombre Vizcondado = "Vizconde"
hombre Baronia = "Baron"

dama :: Titulo -> String
dama Ducado = "Duquesa"
dama Marquesado = "Marquesa"
dama Condado = "Condesa"
dama Vizcondado = "Vizcondesa"
dama Baronia = "Baronesa"

type Territorio = String
type Nombre = String

cba, bsas :: Territorio
cba = "Cordoba"
bsas = "Buenos Aires"

alicia, bob :: Nombre
alicia = "Alicia"
bob = "Bob"

data Genero = Masculino | Femenino
articulo :: Genero -> String
articulo Masculino = "El "
articulo Femenino = "La "

gnr :: Persona -> String
gnr (Rey Masculino) = "Rey"
gnr (Rey Femenino) = "Reina"
gnr (Noble Masculino titulo territorio) = hombre titulo
gnr (Noble Femenino titulo territorio) = dama titulo
gnr (Caballero Masculino nombre) = "Sir "
gnr (Caballero Femenino nombre) = "Lady "
--3c
data Persona = Rey Genero
			 | Noble Genero Titulo Territorio
			 | Caballero Genero Nombre
			 | Aldeano Genero Nombre



tratamiento :: Persona -> String
tratamiento (Rey genero)= "Su majestad " ++ articulo genero ++ " " ++ gnr (Rey genero)
tratamiento (Noble genero titulo territorio) = articulo genero ++ gnr (Noble genero titulo territorio)  ++ " de " ++ territorio
tratamiento (Aldeano genero nombre) = nombre 
tratamiento (Caballero genero nombre) = gnr (Caballero genero nombre) ++ nombre


--3e
--sirs :: [Persona] -> [String]
--sirs [] = []
--sirs (x:xs) | x==Caballero = sirs xs ++ tratamiento x
--			| otherwise = sirs xs

--4a
data IntExp = Const Int
	 | Op IntExp
	 | Plus IntExp IntExp
	 | Times IntExp IntExp
	 | Div Int Int

--eval :: IntExp -> Int
--eval (Op int) = (-1) *  int
--eval (Plus int int2) = int + int2
--eval (Times int int2) = int * int2
--	eval (Div int int2) = div int int2