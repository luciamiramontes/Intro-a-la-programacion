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

-- Ejercicio 2 c
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x >= y && x >= z = x
              | y >= z = y
              | otherwise = z

-- Ejercicio 2 g
-- Tomo el caso cuando hay algun elemento repetido lo sumo una sola vez
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z | (x /= y && x /= z && y /= z) = x + y + z
                    | (x == y && x /= z) = x + z
                    | (y == z && y /= x) = x + y
                    | (x == y && x == z && y == z) = 0

-- Ejercicio 2 i
-- digitoUnidades :: Int -> Int
-- digitoUnidades x | x > 0 = x `mod` 10
--                  | otherwise = ((-1) * x) `mod` 10

-- Ejercicio 2 j
-- digitoDecenas :: Int -> Int
-- digitoDecenas x | x > 0 = div ((x `mod` 100) - digitoUnidades x ) 10
--                 | otherwise = div ((((-1) * x) `mod` 100) - digitoUnidades x) 10

-- Ejercicio 4 b
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a,b) (c,d) | a < c && b < d = True
                      | otherwise = False

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
