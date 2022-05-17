"MATERIA: Introducción a los algoritmos"
"AUTOR: Ana De Pascuale, mail: ana.de.pascuale@mi.unc.edu.ar, github/anadepascuale"

"GUIA 1 y 2, TRABAJAMOS EN HASKELL."

cuadrado :: Int -> Int
cuadrado x = x*x

promedio :: Float -> Float -> Float
promedio a b = (a+b) / 2

f :: Int -> Int
f x = 5 * x

duplica :: Int -> Int
duplica a = a + a

multiplicar :: Int -> Int -> Int
multiplicar zz tt = zz * tt

por2 :: Int -> Int
por2 y = 2 * y

sgn :: Int -> Int
sgn x | x > 0 = 1
      | x < 0 = -1
      | x == 0 = 0
    

entre0y9 :: Int -> Bool
entre0y9 x | x > 9 = False
           | x < 0 = False
           | (x >= 0) && (x <= 9) = True

rangoPrecio :: Int ->  String
rangoPrecio x | (x >= 0) && (x <= 2000) = "muy barato"
              | x > 5000 = "demaciado caro"
              | (x >= 2000) && (x <= 5000) = "hay que verlo bien"
              | x < 0 = "no puede ser!"

absoluto :: Int -> Int
absoluto x | x >= 0 = x
           | x < 0 = (-x)


esMultiplode2 :: Int -> Bool
esMultiplode2 x | mod x 2 == 0 = True
                | mod x 2 /= 0 = False
                

esMultiplode :: Int -> Int -> Bool
esMultiplode a b | mod b a == 0 = True
                  | mod b a /= 0 = False                 

esBisiesto :: Int -> Bool
esBisiesto x | mod x 400 == 0 = True
             | mod x 4 == 0 = True
             | mod x 100 /= 0 = False

dispersion :: Int -> Int -> Int -> Int 
dispersion x y t = (max t (max x y)) - (min t (min x y))



celsiusTofahr :: Float -> Float
celsiusTofahr x = x * 1.8 + 32 


fahrTocelsius :: Float -> Float
fahrTocelsius x = (x - 32) / 1.8


hacefrioF :: Float -> Bool
hacefrioF x | (fahrTocelsius x) < 8 = True  
            | otherwise = False                   


segundo3 :: (Int, Int, Int) -> Int            
segundo3  (x,c,v) = c

ordena :: (Int,Int) -> (Int,Int)

ordena (x,y) |x > y = (y,x)
             |x < y = (x,y)
             |x == y = (x,y)


rangoPrecioParametrizado :: Int -> (Int,Int) -> String
rangoPrecioParametrizado x (y,z) | (x >= 0) && (x < y) = "muy barato"
                                 | (x > z) = "demaciado caro"
                                 | (x > y) && (x < z) = "hay que verlo bien"
                                 | x < 0 = "no puede ser!"

mayor3 :: (Int,Int,Int) -> (Bool, Bool, Bool)
mayor3 (a,b,c) = (a>=3 , b>=3 , c>=3)


todosiguales :: (Int, Int, Int) -> Bool   
todosiguales   (x,c,v) = (x == c && c == v && x == v)      


sp :: [Int] -> [Int] 
sp [] = []
sp (x:xs) | mod x 2 == 0 = x:(sp xs)
          | mod x 2 /= 0 = sp xs


mayores10 :: [Int] -> [Int]
mayores10 [] = []
mayores10 (x:xs) | x > 10 = x:mayores10 xs
                 | otherwise = mayores10 xs


mayoresque :: Int -> [Int] -> [Int]
mayoresque n []=[]
mayoresque n (x:xs) |x >= n = x:(mayoresque n xs)
                    |x < n = (mayoresque n xs)


sumar1 :: [Int] -> [Int]
sumar1 []=[]
sumar1 (x:xs) = (x+1):(sumar1 xs)

dupla :: [Int] -> [Int]
dupla []=[]
dupla (x:xs) = (x*2):(dupla xs)

multiplica :: Int -> [Int] -> [Int]
multiplica z []=[]
multiplica z (x:xs) = (z*x):(multiplica z xs)

todosmenores10 :: [Int] -> Bool
todosmenores10 [] = True
todosmenores10 (x:xs) | x <= 10 && (todosmenores10 xs) = True
                      | otherwise = False

hay0 :: [Int] -> Bool
hay0 [] = False
hay0 (x:xs) | x == 0  = True
            | x/= 0 = False || (hay0 xs)

summ :: [Int] -> Int
summ [] = 0
summ (x:ys) = x + (sum ys) 

repartir :: [String] -> [String] -> [(String,String)]
repartir [] [] = []
repartir (x:xs) [] = []
repartir  [] (y:ys) = []
repartir (x:xs) (y:ys) = (x,y):(repartir xs ys)


apellidos :: [(String, String, String)] -> [String]
apellidos [] = []
apellidos ((a,b,c):xs) = b:(apellidos xs)


cantdelementos :: [Int] -> Int
cantdelementos [] = 0
cantdelementos (x:xs) = 1+ (cantdelementos xs)

indice :: [Int] -> Int -> Int
indice (x:xs) 0 = x
indice (x:xs) n = (indice xs (n-1))

tomar :: [Int] -> Int -> [Int]
tomar [] 0 = []
tomar [] n = []
tomar (x:xs) 0 = []
tomar (x:xs) n = x: (tomar xs (n-1))

tirar :: [Int] -> Int -> [Int]
tirar [] 0 = []
tirar [] n = []
tirar (x:xs) 0 = (x:xs)
tirar (x:xs) n = tirar xs (n-1)

conca :: [Int] -> [Int] -> [Int]
conca [] [] = []
conca [] (y:ys) = (y:ys)
conca (x:xs) [] = (x:xs)
conca (x:xs) (y:ys) = x : (xs ++ (y:ys))

palaizq :: [Int] -> Int -> [Int]
palaizq [] n = [n]
palaizq (x:xs) n = x:(palaizq xs n)

maximo :: [Int] -> Int
maximo [x] = x
maximo (x:(y:xs)) | x >= y = maximo (x:xs)
                  | x < y = maximo (y:xs)
                  
todos0y1 :: [Int] -> Bool
todos0y1 [] = True
todos0y1 (x:xs) | x == 0 || x == 1 =todos0y1 xs 
                | otherwise = False  

repetir :: Int -> Int -> [Int]
repetir 0 k = []
repetir n k = k:(repetir (n-1)k)

solo4o7 :: [Int] -> [Int]
solo4o7 [] = []
solo4o7 (x:xs) | x == 4 = x:solo4o7 xs
               | x == 7 = x:solo4o7 xs
               | otherwise = solo4o7 xs











