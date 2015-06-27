module Genetics (newAllele, twoCutCrossover, rankScale, sigmaScale) where

-- Needed:
--
-- Mutation
-- - simple bit swapping 
-- - permutational allele swapping
-- Crossover
-- - Position based
-- - Order based
-- - One cut
-- - Two cuts
-- - Multiple cuts
-- Fitness evaluation (this is done in another file)
-- - Distance from goal
-- Fitness scaling
-- - Sigma scaling
-- - Rank scaling
-- Selection techniques
-- - Elitism
-- - Roulette Wheel
-- - Stochastic Universal Sampling (Static roulette wheel)
-- - Tournament selection
-- - Niche selection (invented on the spot)
--      this means first group by simmilarity and then choose offspring
--      from each group and also crossbreed
-- Niching (grouping
-- - group by position (better for navigation problems)
-- - chunk group (maybe useful)
-- - group by order (better for permutation problems)

import System.Random
import Data.List

type Allele = Int
type Genotype = [Allele]
type Score = Float
type Generation = [(Genotype, Score)]

allele_count :: Int
allele_count = 5

genome_length :: Int
genome_length = 20

my_gen :: Generation
my_gen = [([0],10),([1],5),([2],7),([3],-1)]

-- This function should be used to get a random new allele 
-- You should apply this with the (!!) operator on the local
-- list of alleles where the current allele is removed so
-- that this function always selects a different allele
newAllele :: Int -> IO Allele
newAllele c = getStdRandom . randomR $ (0, c - 2)

twoCutCrossover :: Genotype -> Genotype -> IO (Genotype, Genotype)
twoCutCrossover g1 g2 = do
    -- get two random cuts
    cut1 <- getStdRandom . randomR $ (0, length g1)
    cut2 <- getStdRandom . randomR $ (0, length g2)
    -- sort the cutsi (f for first cut; s for second cut)
    let f = min cut1 cut2
    let s = max cut1 cut2
    -- swap the area in between 
    return (take f g1 ++ (take (s - f) . drop f $ g2) ++ drop s g1, 
            take f g2 ++ (take (s - f) . drop f $ g1) ++ drop s g2)

sigmaScale :: Generation -> Generation
sigmaScale g = map sigmaScale' g
    where sigmaScale' = (\(a, oldFitness) -> 
              (a, ((oldFitness) - averageFitness) / (2 * sigma)))
          sigma = sqrt variance
          variance = (sum . map (\(_,x) -> 
              (x - averageFitness) ^ (2::Int)) $ g) / 
                     (fromIntegral . length  $ g)
          averageFitness = (sum . map snd $ g) / (fromIntegral . length $ g)

rankScale :: Generation -> Generation
rankScale = rescore 0 . sortBy (\(_,a) (_,b) -> compare a b)
    where rescore _ [] = []
          rescore i ((a,b):xs) = (a,i) : rescore (i + 1) xs

main = putStrLn "Hello world!"
