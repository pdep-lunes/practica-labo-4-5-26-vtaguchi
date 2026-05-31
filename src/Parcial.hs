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
                                rutina :: [Actividad]
                                } deriving Show

type Actividad = (Ejercicio, Int)
type Ejercicio = Perro -> Perro

modificarEnergia :: Perro -> (Int -> Int) -> Perro 
modificarEnergia unPerro unaFuncion = unPerro{ energia = max 0.unaFuncion.energia $ unPerro }

disminuir10Energia :: Int -> Int
disminuir10Energia unaEnergia = unaEnergia - 10

jugar :: Ejercicio
jugar unPerro = modificarEnergia unPerro disminuir10Energia

ladrar :: Int -> Ejercicio
ladrar unaCantLadridos unPerro = modificarEnergia unPerro (aumentarMitadEnergia unaCantLadridos)

aumentarMitadEnergia :: Int -> Int -> Int
aumentarMitadEnergia unaCantLadridos unaEnergia = unaEnergia + div unaCantLadridos 2

regalar :: Juguete -> Ejercicio
regalar unJuguete unPerro = unPerro{ juguetesFav = juguetesFav unPerro ++ [unJuguete]} 

diaDeSpa :: Ejercicio
diaDeSpa unPerro 
    | esRazaExtravagante unPerro || tiempoGuarderia unPerro >= 50 = unPerro {energia = 100, juguetesFav = juguetesFav unPerro ++ ["Peine de Goma"]}
    | otherwise = unPerro

esRazaExtravagante :: Perro -> Bool
esRazaExtravagante unPerro = raza unPerro == "Dálmata" || raza unPerro == "Pomerania"

diaDeCampo :: Ejercicio
diaDeCampo unPerro = unPerro {juguetesFav = drop 1.juguetesFav $ unPerro}

zara :: Perro
zara = UnPerro "Dálmata" ["Pelota", "Mantita"] 90 80

guarderiaDePerritos :: Guarderia
guarderiaDePerritos = UnaGuarderia "Guardería de Perritos" [(jugar, 30), (ladrar 18, 20), (regalar "Pelota", 0), (diaDeSpa, 120), (diaDeCampo, 720)]

-- Parte B
puedeEstarEnGuarderia :: Perro -> Guarderia -> Bool
puedeEstarEnGuarderia unPerro unaGuarderia =  tiempoGuarderia unPerro > tiempoRutina unaGuarderia

tiempoRutina :: Guarderia -> Int
tiempoRutina unaGuarderia = sum.map tiempoActividad.rutina $ unaGuarderia

tiempoActividad :: Actividad -> Int
tiempoActividad (_, tiempo) = tiempo

esPerroResponsable :: Perro -> Bool
esPerroResponsable unPerro = (>3).cantidadJuguetes.diaDeCampo $ unPerro

cantidadJuguetes :: Perro -> Int
cantidadJuguetes unPerro = length.juguetesFav $ unPerro

{-
Que un perro realice una rutina de la guardería (que realice todos sus ejercicios). 
Para eso, el tiempo de la rutina no puede ser mayor al tiempo de permanencia. 
En caso de que esta condición no se cumpla, el perro no hace nada.
-}
realizarRutina :: Guarderia -> Perro -> Perro
realizarRutina unaGuarderia unPerro
    | tiempoRutina unaGuarderia <= tiempoGuarderia unPerro = realizarEjercicios unaGuarderia unPerro
    | otherwise = unPerro

realizarEjercicios :: Guarderia -> Perro -> Perro
realizarEjercicios unaGuarderia unPerro = foldl aplicarEjercicio unPerro (ejerciciosRutina unaGuarderia)

aplicarEjercicio :: Perro -> Ejercicio -> Perro
aplicarEjercicio unPerro unEjercicio = unEjercicio unPerro

ejerciciosRutina :: Guarderia -> [Ejercicio]
ejerciciosRutina unaGuarderia = map ejercicioActividad (rutina unaGuarderia)

ejercicioActividad :: Actividad -> Ejercicio
ejercicioActividad (unEjercicio, _) = unEjercicio

{-
Dados unos perros, reportar todos los que quedan cansados después de realizar la rutina de una guardería. 
Es decir, que su energía sea menor a 5 luego de realizar todos los ejercicios.
-}
perrosCansados :: Guarderia -> [Perro] -> [Perro]
--perrosCansados unaGuarderia unosPerros = filter energiaMenor5 (perrosPostRutina unaGuarderia unosPerros)
perrosCansados unaGuarderia unosPerros = filter energiaMenor5 . perrosPostRutina unaGuarderia $ unosPerros

perrosPostRutina :: Guarderia -> [Perro] -> [Perro]
perrosPostRutina unaGuarderia unosPerros = map (realizarRutina unaGuarderia) unosPerros

energiaMenor5 :: Perro -> Bool
energiaMenor5 unPerro = energia unPerro < 5
