import Control.Exception

fuelForModule mass = 
    mass `div` 3 - 2

fuelForFuel mass =
    if mass <= 0
        then 0
        else mass + fuelForFuel (fuelForModule mass)

refinedFuelForModule = 
    fuelForFuel . fuelForModule

test =
    assert (fuelForModule 12 == 2)
    assert (fuelForModule 14 == 2)
    assert (fuelForModule 1969 == 654)
    assert (fuelForModule 100756 == 33583)
    assert (refinedFuelForModule 12 == 2)
    assert (refinedFuelForModule 14 == 2)
    assert (refinedFuelForModule 1969 == 966)
    assert (refinedFuelForModule 100756 == 50346)
    "Tests passed."

part1 masses =
    sum (map fuelForModule masses)

part2 masses =
    sum (map refinedFuelForModule masses)

readInt str =
    read str :: Int

parseInput input =
    map readInt (lines input)

main = do 
    putStrLn test
    input <- 
        readFile "./inputs/day1.txt"
    putStrLn ("Day 1 -- Part 1: " <> ((show . part1 . parseInput) input))
    putStrLn ("Day 1 -- Part 2: " <> ((show . part2 . parseInput) input))