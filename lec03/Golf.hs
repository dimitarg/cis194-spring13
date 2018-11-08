{-# LANGUAGE LambdaCase #-}

module Golf (skips, localMaxima, histogram)
where

import Data.List
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set


-- ex 1
nths :: Int -> [a] -> [a]
nths n xs = map snd (filter (\a -> (mod (fst a) n == 0)) (zip [1..(length xs)] xs))

skips :: [a] -> [[a]]
skips xs = takeWhile (\x -> length x > 0) (map (\i -> nths i xs) [1..])


-- ex 2
triples :: [a] -> [(a,a,a)]
triples (a:b:c:xs) = (a,b,c):(triples (b:c:xs))
triples _ = []

localMaxima :: [Int] -> [Int]
localMaxima = (map (\case (_,a,_) -> a)) . (filter (\case (a, b, c) -> b > a && b > c)) . triples


-- ex 3
countEqual :: (Ord a) => [a] -> Map a Int
countEqual = foldr (\a -> Map.insertWith (+) a 1) Map.empty

forceKeys :: (Ord a) => [a] -> Map a Int -> Map a Int
forceKeys keys existing =
  let
    new = Map.fromList (map (\x -> (x, 0)) keys)
    keySet = Set.fromList keys
    oldFiltered = Map.filterWithKey (\k _ -> Set.member k keySet) existing
  in Map.unionWith (+) oldFiltered new

histoEntry :: Int -> Int -> String
histoEntry number count = (show number) ++ " = " ++ (unwords (replicate count "*"))

padToLength :: Int -> String -> String
padToLength l a = take l (a ++ (replicate l ' '))

histogram :: [Int] -> String
histogram as =
  let
    histoLines = map (uncurry histoEntry) (Map.toList (forceKeys [0,1,2,3,4,5,6,7,8,9] (countEqual as)))
    len = maximum (map length histoLines)
    padded = map (padToLength len) histoLines
    -- (unlines . reverse . transpose) histoLines
  in unlines $ transpose $ map reverse padded
