import Prelude hiding (length)
import Control.Exception
import Data.List.Split
import Data.Sort
import Data.Maybe

-- Numerical functions --

fallsBetween :: Int -> Int -> Int -> Bool
fallsBetween x0 x1 x =
    (x0 < x && x < x1) || (x1 < x && x < x0)

-- Point definition and functions --

type Point = 
    (Int, Int)

origin :: Point
origin =
    (0, 0)

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) =
    abs (x2 - x1) + abs (y2 - y1)

closestManhattan :: Point -> [Point] -> Int
closestManhattan p ps =
    head . sort $ map (manhattan p) ps

-- Line definition and functions --

data Orientation = 
    Horizontal | Vertical 
    deriving (Eq, Show)

type Line = 
    (Orientation, Point, Int)

opossite :: Orientation -> Orientation
opossite Horizontal =
    Vertical
opossite Vertical = 
    Horizontal 

length :: Line -> Int
length (_, _, length) =
    abs length

start :: Line -> Point
start (_, start, _) =
    start

end :: Line -> Point
end (Horizontal, (x, y), length) = 
    (x + length, y)
end (Vertical, (x, y), length) =
    (x, y + length)

intersect :: Line -> Line -> Maybe Point
intersect (Horizontal, (hx, hy), hl) (Vertical, (vx, vy), vl) =
    if fallsBetween hx (hx + hl) vx && fallsBetween vy (vy + vl) hy
        then Just (vx, hy)
        else Nothing
intersect (Vertical, (vx, vy), vl) (Horizontal, (hx, hy), hl) =
    if fallsBetween hx (hx + hl) vx && fallsBetween vy (vy + vl) hy
        then Just (vx, hy)
        else Nothing
intersect _ _ =
    Nothing

intersects :: [Line] -> [Line] -> [Point] 
intersects ls1 ls2 =
    let
        transform (l1, l2) =
            intersect l1 l2
    in
        catMaybes $ map transform [(l1, l2) | 
            l1 <- ls1,
            l2 <- ls2 ]

stepsTo :: Point -> [Line] -> Int
stepsTo p (l @ (o, _, _) : ls)
    | isJust (intersect l (opossite o, (end (opossite o, p, -1)), 2)) =
        manhattan (start l) p
    | otherwise =
        (length l) + stepsTo p ls

-- Direction definition and functions --

type Direction = 
    (String, Int)

directionToLine :: Point -> Direction -> Line
directionToLine p ("R", length) =
    (Horizontal, p, length)
directionToLine p ("L", length) =
    (Horizontal, p, -length)
directionToLine p ("D", length) =
    (Vertical, p, length)
directionToLine p ("U", length) =
    (Vertical, p, -length)

directionsToLines :: [Direction] -> [Line]
directionsToLines ds =
    let
        collect ls @ (l:_) d =
            directionToLine (end l) d : ls
    in
        tail . reverse $ foldl collect [(Vertical, origin, 0)] ds

parseDirections :: String -> [Direction]
parseDirections input =
    let 
        readInt str =
            read str :: Int
        parseDirection d =
            ([head d], readInt (tail d))
    in
        map parseDirection $ splitOn "," input

parseLines :: String -> [Line]
parseLines input =
    directionsToLines $ parseDirections input

-- Unit tests --

test =
    assert (fallsBetween 1 10 2)
    assert (fallsBetween 12 7 8)
    assert (manhattan origin (3, 4) == 7)
    assert (manhattan origin (-12, 0) == 12)
    assert (closestManhattan origin [(1, 0), (0, 2)] == 1)
    assert (length (Vertical, origin, -10) == 10)
    assert (start (Horizontal, origin, 3) == origin)
    assert (end (Vertical, origin, -3) == (0, -3))
    assert (intersect 
        (Horizontal, (0, 3), 10)
        (Vertical, (5, 0), 10)
        == Just (5, 3))
    assert (stepsTo 
        (3, 3)
        [(Vertical, origin, 3), (Horizontal, (0, 3), 5)]
        == 6)
    assert (directionToLine origin ("U", 5)
        == (Vertical, origin, -5))
    assert (parseLines "U1,D1,L3"
        == [
            (Vertical, origin, -1),
            (Vertical, (0, -1), 1),
            (Horizontal, origin, -3)])
    assert (part1 
        (parseInput "R8,U5,L5,D3\nU7,R6,D4,L4")
        == 6)
    assert (part1 
        (parseInput "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")
        == 159)
    assert (part1 
        (parseInput "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
        == 135)
    assert (part2
        (parseInput "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")
        == 610)
    assert (part2
        (parseInput "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
        == 410)
    "Tests passed."

part1 [lines1, lines2] =
    closestManhattan origin $ intersects lines1 lines2

part2 [lines1, lines2] =
    let 
        transform p =
            stepsTo p lines1 + stepsTo p lines2
    in
        head . sort . map transform $ intersects lines1 lines2

parseInput input =
    map parseLines (lines input)

main = do
    putStrLn test
    input <- 
        readFile "./inputs/day3.txt"
    putStrLn ("Day 3 -- Part 1: " <> ((show . part1 . parseInput) input))
    putStrLn ("Day 3 -- Part 2: " <> ((show . part2 . parseInput) input))