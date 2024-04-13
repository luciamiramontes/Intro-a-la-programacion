doubleMe x = x + x

-- Ejercicio 1 a
f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16

-- Ejercicio 1 b
g :: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1

-- Ejercicio 1 c
-- h = fog
h :: Integer -> Integer
h x = f (g x)

-- k = fog
k :: Integer -> Integer
k x = g (f x)

-- Ejercicio 2 a
absoluto :: Int  -> Int
absoluto x = abs(x)

-- Ejercicio 2 b 
maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y | x >= y = x
                   | otherwise = y

-- Ejercicio 2 c
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x >= y && x >= z = x
              | y >= z = y
              | otherwise = z

-- Ejercicio 2 d
algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | (x == 0) || (y == 0) = True
              | otherwise = False

{- otra manera de hacerlo con pattern matching sería
algunoEs0 :: Float -> Float -> Bool
algunoEs0 0 _ = True
algunoEs0 _ 0 = True
algunoEs0 _ _ = False
-}

-- Ejercicio 2 e
ambosSon0 :: Float -> Float -> Bool
ambosSon0 0 0 = True
ambosSon0 0 _ = False
ambosSon0 _ 0 = False
ambosSon0 _ _ = False

{- ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False 
-}

-- Ejercicio 2 f
mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y = (x <= 3 && y <= 3) || (x > 3 && x <= 7) && (y > 3 && y <= 7) || (x > 7 && y > 7)

-- Ejercicio 2 g
-- Tomo el caso cuando hay algun elemento repetido lo sumo una sola vez
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z | (x /= y && x /= z && y /= z) = x + y + z
                    | (x == y && x /= z) = x + z
                    | (y == z && y /= x) = x + y
                    | (x == y && x == z && y == z) = 0

-- Ejercicio 2 h
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod x y == 0 = True
                 | otherwise = False

-- Ejercicio 2 i
-- digitoUnidades :: Int -> Int
-- digitoUnidades x | x > 0 = x `mod` 10
--                  | otherwise = ((-1) * x) `mod` 10

-- Ejercicio 2 j
-- digitoDecenas :: Int -> Int
-- digitoDecenas x | x > 0 = div ((x `mod` 100) - digitoUnidades x ) 10
--                 | otherwise = div ((((-1) * x) `mod` 100) - digitoUnidades x) 10

-- Ejercicio 3
estanRelacionados :: Int -> Int -> Bool
estanRelacionados x y | mod x y == 0 = True
                      | otherwise = False

-- Ejercicio 4 a
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (x,y) (a,b) = (x*a) + (y*b)

-- Ejercicio 4 b
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a,b) (c,d) | a < c && b < d = True
                      | otherwise = False

-- Ejercicio 4 c
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x,y) (a,b) = sqrt ((a-x)^2 + (b-y)^2)

-- Ejercicio 4 d
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (a,b,c) = a+b+c

-- Ejercicio 4 e
sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos (a,b,c) x | mod a x == 0 && mod b x == 0 && mod c x == 0 = a+b+c
                             | mod a x == 0 && mod b x == 0 && mod c x /= 0 = a+b
                             | mod a x == 0 && mod b x /= 0 && mod c x == 0 = a+c
                             | mod a x == 0 && mod b x /= 0 && mod c x /= 0 = a
                             | mod a x /= 0 && mod b x == 0 && mod c x == 0 = b+c
                             | mod a x /= 0 && mod b x == 0 && mod c x /= 0 = b
                             | mod a x /= 0 && mod b x /= 0 && mod c x == 0 = c

-- Ejercicio 4 f
posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (a,b,c) | even a = 1
                     | even b = 2
                     | even c = 3
                     | otherwise = 4

-- Mismo ejercicio de distinta forma
-- posPrimerPar (a,b,c) | mod a 2 == 0 = 1
--                      | mod b 2 == 0 = 2
--                      | mod c 2 == 0 = 3
--                      | otherwise = 4

-- Ejercicio 4 g
crearPar :: (a) -> (b) -> (a,b)
crearPar a b = (a,b)

--Ejercico 4 h
invertir :: (a,b) -> (b,a)
invertir (a,b) = (b,a)

-- Ejercicio 5 
todosMenores :: (Int, Int, Int) -> Bool
todosMenores (x,y,z) = (j x > l x) && (j y > l y) && (j z > l z) 
-- j y l dos funciones
j :: Int -> Int 
j n | n <= 7 = n^2
    | n > 7 = 2*n - 1

l :: Int -> Int
l n | mod n 2 == 0 = div n 2
    | otherwise = 3*n + 1

-- Ejercicio 6
bisiesto :: Int -> Bool
bisiesto x | (mod x 4 /= 0) || (mod x 100 == 0) && (mod x 400 /= 0) = False
           | otherwise = True

-- Ejercicio 7
distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (a,b,c) (d,e,f) = abs (a - d) + abs (b - e) + abs (c - f)

-- Ejercicio 8
comparar :: Int -> Int -> Int
comparar x y | sumaUltimosDosDigitos x < sumaUltimosDosDigitos y = 1
             | sumaUltimosDosDigitos x > sumaUltimosDosDigitos y = -1
             | sumaUltimosDosDigitos x == sumaUltimosDosDigitos y = 0

digitoUnidades :: Int -> Int
digitoUnidades x = mod (abs x) 10

sacarUnidades :: Int -> Int
sacarUnidades y = div (abs y) 10

digitoDecenas :: Int -> Int
-- a digitoUnidades le aplica sacarUnidades y
digitoDecenas x = digitoUnidades (sacarUnidades x)

sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos x = digitoDecenas x + digitoUnidades x

