import Control.Exception
import Data.List.Split

-- General --

readInt str =
    read str :: Int

pairs [] = 
    []
pairs (x : []) = 
    []
pairs (x : y : zs) = 
    (x, y) : pairs (y : zs)

peekyPairs xs =
    let
        u = -1
        inner prev [] =
            []
        inner prev (x : []) =
            []
        inner prev (x : y : []) =
            (prev, x, y, u) : []
        inner prev (x : y : next : zs) =
            (prev, x, y, next) : inner x (y : next : zs)
    in
        inner u xs

-- Validation --

digitsOf n =
    map (readInt . (: [])) $ show n

pairsOf n =
    pairs $ digitsOf n

peekyPairsOf n =
    peekyPairs $ digitsOf n

digits n =
    length $ digitsOf n

adjacent n =
    let 
        compare (x, y) =
            x == y
    in
        any compare $ pairsOf n

adjacent' n =
    let 
        compare (prev, x, y, next) =
            x == y && prev /= x && next /= x
    in
        any compare $ peekyPairsOf n

increasing n =
    let 
        compare (x, y) =
            x <= y
    in
        all compare $ pairsOf n

valid n =
    digits n == 6 && increasing n && adjacent n

valid' n =
    digits n == 6 && increasing n && adjacent' n

-- Tesintg --

test = 
    assert (digits 123456 == 6)
    assert (adjacent 22)
    assert (adjacent 122)
    assert (not (adjacent 313))
    assert (increasing 11223344)
    assert (not (increasing 12341))
    assert (valid 122345)
    assert (valid 111123)
    assert (not (valid 135679))
    assert (valid 111111)
    assert (not (valid 223450))
    assert (not (valid 123789))
    assert (valid' 112233)
    assert (not (valid' 123444))
    assert (valid' 111122)
    "Tests passed."

part1 [start, end] =
    length $ filter valid [start..end]

part2 [start, end] =
    length $ filter valid' [start..end]

parseInput input =
    map readInt $ splitOn "-" input

main = do 
    putStrLn test
    input <- 
        readFile "./inputs/day4.txt"
    putStrLn ("Day 4 -- Part 1: " <> ((show . part1 . parseInput) input))
    putStrLn ("Day 4 -- Part 2: " <> ((show . part2 . parseInput) input))