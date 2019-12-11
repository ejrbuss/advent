import Control.Exception
import Data.List.Split
import Data.Maybe
import Data.List
import Data.Ord

type Layer =
    ((Int, Int), [Int])

type LayerStack =
    ((Int, Int), [[Int]])

showColors :: Int -> String
showColors 0 =
    " "
showColors 1 =
    "█"

colorOf :: [Int] -> Int
colorOf [] =
    0
colorOf (2 : colors) = 
    colorOf colors
colorOf (color : _) =
    color

pixelsOf :: Layer -> [Int]
pixelsOf (_, pixels) =
    pixels

layerOf :: Int -> Int -> [Int] -> Layer
layerOf width height pixels =
    ((width, height), pixels)

layersOf :: Int -> Int -> [Int] -> [Layer]
layersOf width height pixels =
    map (layerOf width height) $ chunksOf (width * height) pixels

stackLayers :: [Layer] -> LayerStack
stackLayers ((_, []) : _) =
    ((0, 0), [])
stackLayers layers =
    let
        (width, height) =
            fst $ head layers
            
        pixelStack = 
            map (head . pixelsOf) layers

        (_, tailStack) = 
            stackLayers $ map (layerOf width height . tail . pixelsOf) layers
    in
        ((width, height), pixelStack : tailStack)

flattenStack :: LayerStack -> Layer
flattenStack (dim, pixels) =
    (dim, map colorOf pixels)

count :: Int -> Layer -> Int
count n layer = 
    length $ filter (== n) $ pixelsOf layer

flattenedLayerOf :: Int -> Int -> [Int] -> Layer
flattenedLayerOf width height pixels =
    flattenStack $ 
    stackLayers $ 
    layersOf width height pixels

showLayer :: Layer -> String
showLayer ((width, height), pixels) =
    concat $ 
    intersperse "\n" $ 
    map concat $ 
    chunksOf width (map showColors pixels)

test =
    assert ([layerOf 3 2 [1,2,3,4,5,6], layerOf 3 2 [7,8,9,0,1,2]] ==
        layersOf 3 2 (parseInput "123456789012"))
    assert (1 == colorOf [2, 1, 1])
    assert (0 == colorOf [0, 1, 2, 0])
    assert (layerOf 2 2 [0,1,1,0] ==
        flattenedLayerOf 2 2 (parseInput "0222112222120000"))
    "Tests passed."

part1 input =
    let 
        layers =
            layersOf 25 6 input

        zeroCount =
            map (count 0) layers

        zeroLayer =
            layers !! (snd $ minimumBy (comparing fst) (zip zeroCount [0..]))
    in
        count 1 zeroLayer * count 2 zeroLayer

part2 input =
    showLayer $ flattenedLayerOf 25 6 input

readInt str =
    read str :: Int

parseInput input =
    map readInt $ chunksOf 1 input

main = do 
    putStrLn test
    input <- 
        readFile "./inputs/day8.txt"
    putStrLn ("Day 8 -- Part 1: " <> ((show . part1 . parseInput) input))
    putStrLn ("Day 1 -- Part 2:\n" <> ((part2 . parseInput) input))

ls = [layerOf 3 2 [1,2,3,4,5,6], layerOf 3 2 [7,8,9,0,1,2]]