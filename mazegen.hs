{-# OPTIONS_GHC -Wall #-}

module MapGen where

--import Control.Monad
import Numeric.Noise.Perlin
import Data.List
--import System.Random
--import System.IO.Unsafe

data Map = Map [[Square]]

data Square = Wall | Open | Entr | Exit | Path deriving (Show)



main :: IO ()
main = putStrLn (show2DArray (makeBoard 80 1))--((unsafePerformIO $ randomIO::Int) `mod` 10000)))

makeBoard :: Int -> Int -> [[Int]]
makeBoard n s = make2DArray n [mynoise y (x + n * s) n | y <- [1..n], x <- [1..n]]

make2DArray :: Int -> [a] -> [[a]]
make2DArray _ [] = []
make2DArray s arr = (take s arr) : (make2DArray s $ drop s arr)

show2DArray :: [[Int]] -> String
show2DArray a = intercalate "\n" $ map showRow a

showRow :: [Int] -> String
showRow a = concat $ map (showNum) a

showNum :: Int -> String
showNum a
    | a > 2 = "X"
    | otherwise = " "

mynoise :: Int -> Int -> Int -> Int
mynoise y x bs = n
    where seed = 1
          octaves = 5
          scale = 0.13--(0.13 * (60 / (fromIntegral bs)))
          persistance = 0.2 * (10 / (fromIntegral bs))
          perlinNoise = perlin seed octaves scale persistance
          n = round (noiseValue perlinNoise ((fromIntegral y)::Double, (fromIntegral x)::Double, 0) * 10)::Int
