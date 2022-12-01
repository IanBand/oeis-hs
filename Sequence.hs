module Sequence where

import Test.LeanCheck.Stats -- counts
import Control.Applicative
import Data.Bifunctor
import Data.List

-- could make these into one function. Only really want to mess with countsDiffs for now
countsDiffs :: Num a => [(a, a)] -> [a]
countsDiffs c = map (\p -> abs ((fst p) - (snd p))) c

countsSums :: Num a => [(a, a)] -> [a]
countsSums c = map (\p -> abs ((fst p) + (snd p))) c


mapUnmap :: (a -> b) -> (b -> a) -> a -> a
mapUnmap fab fba arg = fba $ fab arg

-- repeatedly 
-- are there any other steady states other than [1] -> [0] -> [1] and [1,0] -> [1,0]?
-- how can we predict the steady state based off of the inital collection?
-- is there a way to visually represent all or a subset of collections?

-- *Sequence> applyNTimes ccd [1,1,1,1,1,1,1,1,1,1,1,1,1,1, 5, 5] 17
-- [0]
-- *Sequence> applyNTimes ccd [1,1,1,1,1,1,1,1,1,1,1,1,1,1, 5, 6] 17
-- [1]
-- proof that steady state is not only based on parity of inital sum as I initally suspected 

ccd :: [Int] -> [Int]
ccd a = mapUnmap counts countsDiffs a

applyNTimes :: (a -> a) -> a -> Int -> a
applyNTimes _ init 0 = init
applyNTimes f init n = applyNTimes f (f init) (n - 1)


--applyUntilSteadyState :: Eq a => a -> (a -> a) -> Int -> Either Int a


-- below are some sequences for exploration

 -- size of largest grouping of counts differences
doubleCountSeq :: [Int] -> Int
doubleCountSeq l = 
    let groups = counts $ countsDiffs $ counts l 
    in snd (maximumBy (\a b -> compare (snd a) (snd b) ) groups)

-- largest count of any number seen so far, if same as before, duplicate prev term but subtract 1
lessMostSeq :: [Int] -> Int
lessMostSeq l = if nextNum == last l then nextNum - 1 else nextNum
    where nextNum = snd (maximumBy (\a b -> compare (snd a) (snd b) ) (counts l))
    

rseq :: ([Int] -> Int) -> Int -> Int -> [Int]
rseq _ seed 1 = [seed]
rseq nextf seed n 
    = prevSeq ++ [nextf prevSeq] -- prob a better way to append
    where 
        prevSeq = rseq nextf seed ( n - 1 ) 




    