{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.List hiding (concat)
--import Data.Char
import Data.Maybe(catMaybes)
import System.Random
--import qualified Data.Foldable as F
--import Control.Monad

textMap :: [String]
textMap = ["XXXXXXXXXXXXXXX"
          ,"3 X     XXX   X"
          ,"X       XXX   X"
          ,"X   XXX  X    X"
          ,"X   XXX     X X"
          ,"XX  XXX     X X"
          ,"X    X    XXX X"
          ,"X XX   X      2"
          ,"X XX   X      X"
          ,"XXXXXXXXXXXXXXX"]

myMap :: Map
myMap = readMap textMap

-- Maybe version of (!!)
elemAt :: [a] -> Int -> Maybe a
elemAt xs i
    | i < 0 || i + 1 > length xs = Nothing
    | otherwise = Just $ xs !! i

-------- MAP --------

data Map = Map [[Square]]
instance Show Map where
    show (Map m) = unlines . (map $ concat . (map show)) $ m

readMap :: [String] ->  Map
readMap = Map . readMap'
    where readMap' [] = []
          readMap' (x:xs) = map readSquare x : readMap' xs

data Square = Exit | Entr | Open | Wall | Walk deriving (Eq)
instance Show Square where
    show Exit = "3"
    show Entr = "2"
    show Open = " "
    show Wall = "X"
    show Walk = "."

readSquare :: Char -> Square
readSquare '3' = Exit
readSquare '2' = Entr
readSquare ' ' = Open
readSquare 'X' = Wall
readSquare '.' = Walk
readSquare  _  = error "not a valid square"

type Pos = (Int, Int)
--data Direction = North | South | East | West deriving (Show, Eq)
type Route = [Direction]
type Direction = (Int, Int)

north :: (Int, Int)
south :: (Int, Int)
east  :: (Int, Int)
west  :: (Int, Int)
north = (-1, 0)
south = ( 1, 0)
east  = ( 0, 1)
west  = ( 0,-1)

--move :: Direction -> Pos -> Map -> Pos
--move d p (Map m) = if (fromJust (m `elemAt` fst (add d p)) `elemAt` snd (add d p)) 
--                      `elem` map Just [Open, Exit, Entr] then (add d p) else p
--    where add :: Direction -> Pos -> Pos
--          add North (r, c) = (r - 1, c)
--          add South (r, c) = (r + 1, c)
--          add East  (r, c) = (r, c + 1)
--          add West  (r, c) = (r, c - 1)

move :: Pos -> (Int, Int) -> Map -> Maybe Pos
move p d (Map m) = maybe Nothing
                          (\x -> if x == Wall then Nothing else Just newPos)
                          (maybe Nothing (`elemAt` snd newPos) (m `elemAt` (fst newPos)))
    where newPos = (fst d + fst p, snd d + snd p)

startPos :: Map -> Pos
startPos (Map m) = (head . catMaybes . map (Entr `elemIndex`) . transpose $ m, 
                    head . catMaybes . map (Entr `elemIndex`) $ m)

exitPos :: Map -> Pos
exitPos (Map m) = (head . catMaybes . map (Exit `elemIndex`) . transpose $ m, 
                   head . catMaybes . map (Exit `elemIndex`) $ m)

distance :: Pos -> Pos -> Int
distance (r1, c1) (r2, c2) = abs (r1 - r2 + c1 - c2)

evaluate :: Pos -> Route -> Map -> Maybe Pos
evaluate p [] _ = Just p
evaluate p (d:ds) m = maybe Nothing (\x -> evaluate x ds m) (move d p m)

onMap :: Route -> Map -> Maybe Map
onMap route mp = onMap' (startPos mp) route mp
    where onMap' _ [] m = Just m
          onMap' p (d:ds) m = maybe Nothing (\x -> onMap' x ds (changeMap x Walk m)) (move p d m)

changeMap :: Pos -> Square -> Map -> Map
changeMap p s (Map m) = Map $ (take (fst p) m) ++ 
                              (take (snd p) (m !! fst p) ++ 
                              [s] ++ 
                              drop (snd p + 1) (m !! fst p)) : 
                              drop (fst p + 1) m 

-------- GENETICS --------

type Genotype = [Bool]
type Genome = (Genotype, Fitness)
type Fitness = Int
type Generation = [Genome]

mate_count :: Int
mate_count = 70

child_count :: Int
child_count = 2

mutation_chance :: Float
mutation_chance = 0.01

crossover_rate :: Float
crossover_rate = 0.6

chromo_length :: Int
chromo_length = 60

change :: Fitness -> Int
change = div 1 . flip (^) (2::Int) . (+ 1) -- 1 / ((x + 1) ^ 2)

most_fit :: Fitness
most_fit = 1

least_fit :: Fitness
least_fit = 100

myGen :: Generation
myGen = [(replicate (chromo_length) True, 100), (replicate (chromo_length) False, 100)]

encode :: Route -> Genotype
encode [] = []
encode ((-1, 0):xs) = [False, False] ++ encode xs
encode (( 0, 1):xs) = [False, True]  ++ encode xs
encode (( 0,-1):xs)  = [True, False]  ++ encode xs
encode (( 1, 0):xs)  = [True, True]   ++ encode xs
encode _ = error "not valid route"

decode :: Genotype -> Route
decode [] = []
decode (False:False:xs) = (north : (decode xs))
decode (False:True :xs) = (east : (decode xs))
decode (True :False:xs) = (west : (decode xs))
decode (True :True :xs) = (south : (decode xs))
decode [_] = error "not a valid genome"

testGenotype :: Genotype -> Map -> Fitness
testGenotype g m = maybe
    least_fit
    (distance (exitPos m))
    (evaluate (startPos m) (decode g) m)

timeline :: Generation -> IO ()
timeline g = do
    putStrLn . show . map (snd) $ g
    if (most_fit ==) . maximum . map snd $ g then 
        --(putStrLn . show . decode . fst . maximumBy (\a b -> compare (snd a) $ snd b) $ g)
        putStrLn . show $ 
            (onMap . decode . fst . (maximumBy (\a b -> compare (snd a) $ snd b))) g myMap else
        nextGen g myMap >>= timeline

nextGen :: Generation -> Map -> IO Generation
nextGen g m = fmap concat . sequence . replicate mate_count $ 
    roulette g >>= (\mum -> 
        roulette g >>= (\dad -> mate mum dad)) >>= (\next_gen -> 
            return $ zip (next_gen) $ (map (flip testGenotype m) $ next_gen))

roulette :: Generation -> IO Genome
roulette gen = fmap (roulette' gen) . getStdRandom . randomR $ (0, sum . map (change . snd) $ gen)
    where roulette' :: Generation -> Fitness -> Genome
          roulette' [] _ = error "empty generation"
          roulette' [g] _ = g
          roulette' (g:gs) n
              | (0 >=) . (n -) . change . snd $ g = g
              | otherwise = roulette' gs . (n -) . change . snd $ g

mate :: Genome -> Genome -> IO [Genotype]
mate dad mum = (crossover (fst dad) (fst mum)) >>= (\x -> sequence $ map mutate x)

mutate :: Genotype -> IO Genotype
mutate g = sequence . (map mutate') $ g
    where mutate' :: Bool -> IO Bool
          mutate' a = (getStdRandom . randomR $ (0,1::Float)) >>= (\x -> 
              if x < mutation_chance then return (not a) else return a)

crossover :: Genotype -> Genotype -> IO [Genotype]
crossover dad mum = (getStdRandom . randomR $ (0,1::Float)) >>= (\x ->
    if x > crossover_rate || mum == dad then return [mum, dad] else 
    (getStdRandom . randomR $ (0::Int, chromo_length - 1)) >>= (\cp ->
        return [take cp mum ++ drop cp dad, take cp dad ++ drop cp mum]))

------- IO -------

main :: IO ()
main = timeline myGen
