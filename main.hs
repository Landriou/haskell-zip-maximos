
volumenEsfera :: Double -> Double
volumenEsfera radius =  4.0/3.0 * 3.14 * radius^3 

sumaCoins :: Int -> Int -> Int -> Int -> Int -> Int
sumaCoins a b c d e = a + b * 5 + c * 10 + d * 50 + e * 100

incrementTuple :: (Int, Int, Int) ->  (Int, Int, Int)
incrementTuple (x,y,z) = (x+1,y+1,z+1)

getSquare:: Int -> Int
getSquare x = x * x

calcularMax:: Int -> Int -> Int -> Int
calcularMax x y z = max (max x y) z

cuadrante:: (Double,Double) -> String
cuadrante (x,y) | x > 0 && y > 0 = "Primer cuadrante"
                | x < 0 && y > 0 = "Segundo cuadrante"
                | x < 0 && y < 0 = "Tercer cuadrante"             
                | x > 0 && y < 0 = "Cuarto cuadrante"

divisores:: Int -> [Int]
divisores n = [x | x <- [-n..n],x/=0 ,n `mod` x == 0]

factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]

toTime :: Int -> [Int]
toTime x = [toHour x,toMinutes (x - (toHour x)*3600),x - ((toHour x)*3600) - toMinutes (x - (toHour x)*3600)*60]
--, s <- 0
toHour :: Int -> Int
toHour x = (x `div` 60) `div` 60 

toMinutes :: Int -> Int
toMinutes x = (x `div` 60)

tomar :: Int -> [a] -> [a]
tomar 0 a = []
tomar n (h:t) = [h]++ tomar (n-1) t

nIndex :: [Int] -> Int -> Int
nIndex (h:t) 1 = h
nIndex (h:t) n = nIndex t (n-1)

elem2 :: [Int] -> Int -> Bool
elem2 [] e = False
elem2 (h:t) e = (elem2 t e) || (h == e)

-------- 2.10----

data Complejo = Complejo Int Int deriving (Show)

sumaComplejo :: Complejo -> Complejo -> Complejo
sumaComplejo (Complejo a b) (Complejo c d) = (Complejo (a+c) (b+d))

------ 2.11 -----
data Color = Azul | Rojo | Verde | Lila| Naranja| Marron | Blanco | Negro | Celeste | Amarillo deriving (Enum,Show)  

---- 3.1 ---
sumaPar :: [(Int,Int)] -> [Int]
sumaPar [] = []
sumaPar ((i,j):t) = (i + j) : sumaPar t

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos (h1:t1) (h2:t2) = zipMaximos t1 t2 : [x | x <- [max h1 h2]] 

main :: IO ()
main =  do
        print(volumenEsfera 5)
        print(sumaCoins 1 1 1 1 1)
        print(incrementTuple (1,1,1))
        print(getSquare 2)
        print(calcularMax 1 2 3)
        print(cuadrante (1,-1))
        print(divisores 7)
        print(toTime 3660)
        print(tomar 3 [1,2,3,4,5,6,7])
        print(nIndex [1,2,3,4,5,6,7] 1)
        print(elem2 [1,2,3,4,5,6,7] 5)
        print(sumaComplejo (Complejo 1 2) (Complejo 2 3))
        print([Azul,Amarillo,Rojo])
        print(sumaPar [(1,2),(2,3),(3,2)])
        --print(max 1 3)
        print(zipMaximos [1,2,3,4,5,6,7] [7,5,5,5,5,5,3])