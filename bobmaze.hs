module Main where

import Data.List
import Data.Char
import Data.Maybe(catMaybes, fromJust)
import System.Random
import System.IO.Unsafe

bestGenotype = encode [East, South, South]

myMap = Map 
    [
        replicate 15 Wall,
        [Exit, Open, Wall] ++ replicate 5 Open ++ replicate 3 Wall ++ replicate 3 Open ++ [Wall],
        Wall : replicate 7 Open ++ replicate 3 Wall ++ replicate 3 Open ++ [Wall],
        Wall : replicate 3 Open ++ replicate 3 Wall ++ [Open, Open, Wall] ++ replicate 4 Open ++ [Wall],
        Wall : replicate 3 Open ++ replicate 3 Wall ++ replicate 5 Open ++ [Wall, Open, Wall],
        [Wall, Wall, Open, Open] ++ replicate 3 Wall ++ replicate 5 Open ++ [Wall, Open, Wall],
        Wall : replicate 4 Open ++ [Wall] ++ replicate 4 Open ++ replicate 3 Wall ++ [Open, Wall],
        [Wall, Open, Wall, Wall] ++ replicate 3 Open ++ [Wall] ++ replicate 6 Open ++ [Entr],
        [Wall, Open, Wall, Wall] ++ replicate 3 Open ++ [Wall] ++ replicate 6 Open ++ [Wall],
        replicate 15 Wall        
    ]

-- Maybe version of (!!)
elemAt :: [a] -> Int -> Maybe a
elemAt xs i
    | i < 0 || i + 1 > length xs = Nothing
    | otherwise = Just $ xs !! i

-------- MAP --------

data Map = Map [[Square]]
instance Show Map where
    show (Map m) = intercalate "\n" $ map (foldl1 (++)) $ map (map show) m

showSqr :: Square -> String
showSqr Exit = "3"
showSqr Entr = "2"
showSqr Wall = "1"
showSqr Open = "0"

data Square = Exit | Entr | Open | Wall deriving (Eq)
instance Show Square where
    show Exit = "3"
    show Entr = "2"
    show Open = "0"
    show Wall = "1"

type Pos = (Int, Int)
data Direction = North | South | East | West deriving (Show, Eq)
type Route = [Direction]
 
move :: Direction -> Pos -> Map -> Pos
move d p (Map m) = if ((fromJust (m `elemAt` fst (add d p))) `elemAt` snd (add d p)) 
                      `elem` map Just [Open, Exit, Entr] then (add d p) else p
   where 
      add North (r, c) = (r - 1, c)
      add South (r, c) = (r + 1, c)
      add East  (r, c) = (r, c + 1)
      add West  (r, c) = (r, c - 1)

startPos :: Map -> Pos
startPos (Map m) = (head $ catMaybes $ map (Entr `elemIndex`) (transpose m), 
                    head $ catMaybes $ map (Entr `elemIndex`) m)

exitPos :: Map -> Pos
exitPos (Map m) = (head $ catMaybes $ map (Exit `elemIndex`) (transpose m), 
                   head $ catMaybes $ map (Exit `elemIndex`) m)

distance :: Pos -> Pos -> Int
distance (r1, c1) (r2, c2) = abs (r1 - r2 + c1 - c2)

evaluate :: Route -> Pos -> Map -> Pos
evaluate [] p m = p
evaluate (d:ds) p m = evaluate ds (move d p m) m

-------- GENETICS --------

type Genotype = [Bool]
type Genome = (Genotype, Fitness)
type Fitness = Int
type Generation = [Genome]

encode :: Route -> Genotype
encode [] = []
encode (North:xs) = [False, False] ++ encode xs
encode (East:xs) = [False, True]  ++ encode xs
encode (West:xs)  = [True, False]  ++ encode xs
encode (South:xs)  = [True, True]   ++ encode xs

decode :: Genotype -> Route
decode [] = []
decode (False:False:xs) = (North : (decode xs))
decode (False:True :xs) = (East : (decode xs))
decode (True :False:xs) = (West : (decode xs))
decode (True :True :xs) = (South : (decode xs))
decode [_] = error "not a valig genome"

determineFitness :: Pos -> Map -> Fitness
determineFitness p (Map m) = (distance (startPos (Map m)) (exitPos (Map m))) `div` 
                             ((distance p (exitPos (Map m))) + 1)

testGenotype :: Genotype -> Map -> Fitness
testGenotype g (Map m) = determineFitness (evaluate (decode g) (startPos (Map m)) (Map m)) (Map m)

------- IO -------

main = do
    next_gen <- nextGen myGeneration myMap
    next_gen2 <- nextGen next_gen myMap
    putStrLn $ show $ map (snd) $ next_gen2

-------- Notes ---------
-- Make all these strict: 

children_count :: Int
children_count = 2

mutation_chance :: Float
mutation_chance = 0.1

crossover_rate :: Float
crossover_rate = 0.7

genome_length = 20

myGeneration :: Generation
myGeneration = [(replicate (genome_length * 2) True, 0), (replicate (genome_length * 2) False, 1)]


nextGen :: Generation -> Map -> IO Generation
nextGen g m = do
    mum <- roulette g
    dad <- roulette g
    next_gen <- mate mum dad
    return $ zip (next_gen) $! (map (flip testGenotype m) $ next_gen)

weightedGenList :: Generation -> [Genotype]
weightedGenList g = foldl1' (++) [replicate (ft + 1) gt | gt <- map fst g, ft <- map snd g] 

roulette :: Generation -> IO Genotype
roulette g = do
    rand <- getStdRandom $ randomR (0::Int, (length g + sum (map snd g))::Int)
    return $ (weightedGenList g) !! rand

mate :: Genotype -> Genotype -> IO [Genotype]
mate dad mum = do 
    children <- crossover dad mum
    sequence $ map mutate children

mutate :: Genotype -> IO Genotype
mutate g = sequence $ map mutate' g

mutate' :: Bool -> IO Bool
mutate' a = do
    rand <- getStdRandom $ randomR(0::Float, 1::Float)
    if (rand < mutation_chance) then return (not a) else return a

crossover :: Genotype -> Genotype -> IO [Genotype]
crossover dad mum = if 
    (unsafePerformIO $! getStdRandom $ randomR (0::Float,1::Float)) > crossover_rate || 
    mum == dad then 
        return [mum, dad] 
    else do
        cp <- getStdRandom $ randomR(0::Int, ((genome_length * 2) - 1)::Int)
        return [take cp mum ++ drop cp dad, take cp dad ++ drop cp mum]
