import Prelude
import qualified Data.Map as Map
import Control.Exception
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Tuple

type Orbits = 
    Map.Map String String
type Orbit = 
    (String, String) 

insertOrbit :: Orbit -> Orbits -> Orbits
insertOrbit (inside, outside) orbits =
    Map.insert outside inside orbits

toOrbits :: [Orbit] -> Orbits
toOrbits [] = 
    Map.empty
toOrbits (o:os) = 
    insertOrbit o (toOrbits os)

orbitsOf :: String -> Orbits -> Int
orbitsOf object orbits =
    case Map.lookup object orbits of
        Just nextObject ->
            1 + orbitsOf nextObject orbits
        Nothing ->
            0

allOrbitsOf :: Orbits -> Int
allOrbitsOf orbits =
    sum $ map (`orbitsOf` orbits) $ Map.keys orbits

pathOf :: String -> Orbits -> [String]
pathOf object orbits =
    case Map.lookup object orbits of
        Just nextObject ->
            object : pathOf nextObject orbits
        Nothing ->
            [object]

pathTo :: String -> String -> Orbits -> [String]
pathTo src dest orbits =
    nub $ 
        (pathOf src orbits \\ pathOf dest orbits) ++
        (pathOf dest orbits \\ pathOf src orbits)

parseOrbit :: String -> Orbit
parseOrbit input =
    case splitOn ")" input of 
        (inside:outside:_) -> (inside, outside) 

parseInput :: String -> Orbits
parseInput input =
    toOrbits $ map parseOrbit $ lines input

test =
    let
        exOrbit =
            ("Sun", "Earth")
        exOrbits =
            [("Sun", "Earth"), ("Earth", "Moon")]
        exInput =
            parseInput "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
    in
    assert (insertOrbit exOrbit Map.empty == Map.fromList [(swap exOrbit)])
    assert (toOrbits exOrbits == Map.fromList (map swap exOrbits))
    assert (orbitsOf "Earth" (toOrbits exOrbits) == 1)
    assert (orbitsOf "Moon" (toOrbits exOrbits) == 2)
    assert (orbitsOf "D" exInput == 3)
    assert (orbitsOf "L" exInput == 7)
    assert (orbitsOf "COM" exInput == 0)
    assert (allOrbitsOf exInput == 42)
    assert (pathOf "H" exInput == ["H", "G", "B", "COM"])
    assert (length (pathTo "L" "I" exInput) == 5)
    assert (length (pathTo "F" "H" exInput) == 6)
    assert (length (pathTo "L" "COM" exInput) == 7)
    "Tests passed."

part1 = 
    allOrbitsOf

part2 input =
    length $ pathTo 
        (fromJust $ Map.lookup "YOU" input) 
        (fromJust $ Map.lookup "SAN" input) 
        input

main = do 
    putStrLn test
    input <- 
        readFile "./inputs/day6.txt"
    putStrLn ("Day 6 -- Part 1: " <> ((show . part1 . parseInput) input))
    putStrLn ("Day 6 -- Part 2: " <> ((show . part2 . parseInput) input))