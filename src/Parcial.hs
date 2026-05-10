module Parcial where
import Text.Show.Functions()

data Perro = UnPerro {  raza :: String,
                        juguetesFav :: [Juguete],
                        tiempoGuarderia :: Int,
                        energia :: Int
                    } deriving Show

type Juguete = String

data Guarderia = UnaGuarderia {
                                nombre :: String,
                                rutina :: [Actividades]
                                } deriving Show

type Actividades = (Ejercicio, Int)
type Ejercicio = Perro -> Perro

modificarEnergia :: Perro -> (Int -> Int) -> Perro 
modificarEnergia unPerro unaFuncion = unPerro{ energia = max 0.unaFuncion.energia $ unPerro }

disminuir10Energia :: Int -> Int
disminuir10Energia unaEnergia = unaEnergia - 10

jugar :: Perro -> Perro
jugar unPerro = modificarEnergia unPerro disminuir10Energia

ladrar :: Int -> Perro -> Perro
ladrar unaCantLadridos unPerro = modificarEnergia unPerro (aumentarMitadEnergia unaCantLadridos)

aumentarMitadEnergia :: Int -> Int -> Int
aumentarMitadEnergia unaCantLadridos unaEnergia = unaEnergia + div unaCantLadridos 2

regalar :: Juguete -> Perro -> Perro
regalar unJuguete unPerro = unPerro{ juguetesFav = juguetesFav unPerro ++ [unJuguete]} 

diaDeSpa :: Perro -> Perro
diaDeSpa unPerro 
    | esRazaExtravagante unPerro || tiempoGuarderia unPerro >= 50 = unPerro {energia = 100, juguetesFav = juguetesFav unPerro ++ ["Peine de Goma"]}
    | otherwise = unPerro

esRazaExtravagante :: Perro -> Bool
esRazaExtravagante unPerro = raza unPerro == "Dálmata" || raza unPerro == "Pomerania"

diaDeCampo :: Perro -> Perro
diaDeCampo unPerro = unPerro {juguetesFav = drop 1.juguetesFav $ unPerro}

zara :: Perro
zara = UnPerro "Dálmata" ["Pelota", "Mantita"] 90 80

guarderiaDePerritos :: Guarderia
guarderiaDePerritos = UnaGuarderia "Guardería de Perritos" [(jugar, 30), (ladrar 18, 20), (regalar "Pelota", 0), (diaDeSpa, 120), (diaDeCampo, 720)]

puedeEstarEnGuarderia :: Perro -> Guarderia -> Bool
puedeEstarEnGuarderia unPerro unaGuarderia =  tiempoGuarderia unPerro > totalTiempoRutina unaGuarderia

totalTiempoRutina :: Guarderia -> Int
totalTiempoRutina unaGuarderia = sum.map tiempoRutina.rutina $ unaGuarderia

tiempoRutina :: Actividades -> Int
tiempoRutina (_, tiempo) = tiempo

esPerroResponsable :: Perro -> Bool
esPerroResponsable unPerro = (>3).cantidadJuguetes.diaDeCampo $ unPerro

cantidadJuguetes :: Perro -> Int
cantidadJuguetes unPerro = length.juguetesFav $ unPerro