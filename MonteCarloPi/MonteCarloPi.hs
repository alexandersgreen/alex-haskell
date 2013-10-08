module MonteCarloPi where

import System.Random
import Data.IORef

-- define a data type to represent a point
type Point = (Double,Double)

-- define a data type to represent a square
-- the single Double argument is the dimension of the square
type Square = Double
-- the corner of a square is always at 0.0,0.0

-- define a data type to represent a circle
-- the single Double argument is the radius of the circle
type Circle = Double
-- the middle of a circle is always at radius,radius

-- function to define a circle that fits exactly within a square
-- I.e. a circle with a radius equal to half the dimension of the square
circleInSquare :: Square -> Circle
circleInSquare s = s/2.0

-- the area of a square is dimension^2
-- the area of a circle is pi*r^2
-- in a circle that fits in a sqaure, the radius is dimension/2.0, so the area is pi * (dimension/2.0)^2
-- = pi/4.0 * dimension^2
-- so pi = (area of circle * 4.0)/ area of square

-- randomly choose points in the square
-- pi is approximately 4 times (the number of these points that are also in the circle, divided by the total number of points)

-- function to return whether a point sits within a circle
isInCircle :: Point -> Circle -> Bool
isInCircle (x,y) r = (x-r)^2 + (y-r)^2 <= r^2

-- a montecarlo method for approximating pi
approxPi' :: (Int,Double) -> Double -> IO (Int,Double)
approxPi' (runs,pi) n = do x <- randomRIO (0,n)
                           y <- randomRIO (0,n)
                           if (isInCircle (x,y) (circleInSquare n)) 
                              then (return (runs+1,((pi*(fromIntegral runs))+1.0)/((fromIntegral runs)+1.0))) 
                              else (return (runs+1,(pi*(fromIntegral runs))/((fromIntegral runs)+1.0)))
                     
--repeats the montecarlo method indefinitely using a square of the given dimension
approxPiR :: Double -> IO ()
approxPiR square = do approxPiR' (0,0.0) square

approxPiR' :: (Int,Double) -> Double -> IO ()
approxPiR' (n,pi) d = do (n',pi') <- approxPi' (n,pi) d
                         putStrLn (show (4.0*pi'))
                         approxPiR' (n',pi') d

--repeats the montecarlo method the given number of times using a square of the given dimension
approxPi :: Double -> Int -> IO Double
approxPi square runs = do pi <- approxPi'' runs square (0,0.0)
                          return (4.0*pi)

approxPi'' :: Int -> Double -> (Int,Double) -> IO Double
approxPi'' 0 _ (_,pi) = return pi
approxPi'' runs d (n,pi) = do (n',pi') <- approxPi' (n,pi) d
                              if (runs `mod` 100000 == 0) then putStrLn ((show runs)++" : "++(show (4.0*pi')))
                                                         else return ()
                              approxPi'' (runs-1) d (n',pi')
                           


---testing functions
type Strings = [String]

squareNum :: Int
squareNum = 28

circ :: (Int,Int) -> Char
circ (x,y) = if (isInCircle (fromIntegral x,fromIntegral y) (circleInSquare (fromIntegral squareNum))) then '#' else '*'

circle' :: String
circle' = [circ (x,y) | x <- [0..squareNum], y <- [0..squareNum]]

circle'' :: [(Int,Int)]
circle'' = [(x,y) | x <- [0..squareNum], y <- [0..squareNum]]

partition :: [a] -> [[a]]
partition [] = []
partition xs = ((take (squareNum+1) xs):(partition (drop (squareNum+1) xs)))

circle :: Strings
circle = partition circle'

draw :: Strings -> String
draw [] = []
draw (x:xs) = x ++ "\n" ++ draw xs

testCircle :: IO ()
testCircle = do putStrLn (draw circle)





