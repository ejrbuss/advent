import Control.Exception
import Data.List.Split
import Data.Sequence

data Mode = 
    Position | Immediate
    deriving (Eq, Show)

data Op =
    Add | Mul | In | Out | Jt | Jf | Lt | Eq | Exit
    deriving (Eq, Show)

type Instruction =
    (Op, Mode, Mode)

decode :: Int -> Instruction
decode int =
    let
        decodeOp 1 = 
            Add
        decodeOp 2 = 
            Mul
        decodeOp 3 = 
            In
        decodeOp 4 = 
            Out
        decodeOp 5 =
            Jt
        decodeOp 6 =
            Jf
        decodeOp 7 =
            Lt
        decodeOp 8 =
            Eq
        decodeOp 99 = 
            Exit

        decodeMode 0 =
            Position
        decodeMode 1 =
            Immediate

        op = 
            decodeOp (int `mod` 100)
        mode1 =
            decodeMode ((int `div` 100) `mod` 10)
        mode2 =
            decodeMode ((int `div` 1000) `mod` 10)
    in 
        (op, mode1, mode2)
        

eval :: Seq Int -> Int -> [Int] -> [Int] -> [Int]
eval memory pc input output =
    let 
        param Immediate offset =
            index memory (pc + offset)
        param Position offset =
            index memory (param Immediate offset)

        write offset value =
            (update (param Immediate offset) value memory)

        -- Adds the values of its firsta nd second parameters and stores them
        -- at its 3rc parameter
        opcode (Add, mode1, mode2) = 
            eval 
                (write 3 (param mode1 1 + param mode2 2))
                (pc + 4)
                input
                output

        -- Multiplies the values of its firsta nd second parameters and stores 
        -- them at its 3rc parameter
        opcode (Mul, mode1, mode2) =
            eval
                (write 3 (param mode1 1 * param mode2 2))
                (pc + 4)
                input
                output

        -- Takes a single integer as input and saves it to the position given
        -- by its only parameter
        opcode (In, _, _) =
            eval
                (write 1 (head input))
                (pc + 2)
                (tail input)
                output

        -- Ouputs the value of its only parameter
        opcode (Out, mode1, _) =
            eval
                memory
                (pc + 2)
                input
                ((param mode1 1) : output)

        -- If the first parameter is non-zero, it sets pc to the value of the
        -- second parameter, otherwise it does nothing
        opcode (Jt, mode1, mode2) =
            eval
                memory
                (if (param mode1 1) /= 0
                    then (param mode2 2) 
                    else pc + 3)
                input
                output

        -- If the first parameter is zero, it sets pc to the value of the
        -- second parameter, otherwise it does nothing
        opcode (Jf, mode1, mode2) =
            eval
                memory
                (if (param mode1 1) == 0 
                    then (param mode2 2) 
                    else pc + 3)
                input
                output

        -- If the first parameter is less than the second, store 1 in the 
        -- position given by its third parameter, otherwise store 0
        opcode (Lt, mode1, mode2) =
            eval
                (if (param mode1 1) < (param mode2 2) 
                    then (write 3 1)
                    else (write 3 0))
                (pc + 4)
                input
                output

        -- If the first parameter is equal to the second, store 1 in the 
        -- position given by its third parameter, otherwise store 0
        opcode (Eq, mode1, mode2) =
            eval
                (if (param mode1 1) == (param mode2 2) 
                    then (write 3 1)
                    else (write 3 0))
                (pc + 4)
                input
                output

        -- Exits the program and returns the output
        opcode (Exit, _, _) =
            output
    in
        opcode $ decode $ param Immediate 0

evalStr str input =
    head $ eval (parseInput str) 0 input [] 

test =
    assert (decode 1002 == (Mul, Position, Immediate))
    assert (evalStr "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [0] == 0)
    assert (evalStr "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [-5] == 1)
    assert (evalStr "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [0] == 0)
    assert (evalStr "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [27] == 1)
    assert (evalStr "3,9,8,9,10,9,4,9,99,-1,8" [6] == 0)
    assert (evalStr "3,9,8,9,10,9,4,9,99,-1,8" [8] == 1)
    assert (evalStr "3,9,7,9,10,9,4,9,99,-1,8" [8] == 0)
    assert (evalStr "3,9,7,9,10,9,4,9,99,-1,8" [-12] == 1)
    assert (evalStr "3,3,1108,-1,8,3,4,3,99" [101] == 0)
    assert (evalStr "3,3,1108,-1,8,3,4,3,99" [8] == 1)
    assert (evalStr "3,3,1107,-1,8,3,4,3,99" [9] == 0)
    assert (evalStr "3,3,1107,-1,8,3,4,3,99" [-2] == 1)
    assert (evalStr
        ("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31," <>
        "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104," <>
        "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99") [3] ==  999)
    assert (evalStr
        ("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31," <>
        "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104," <>
        "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99") [8] ==  1000)
    assert (evalStr
        ("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31," <>
        "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104," <>
        "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99") [1000] ==  1001)
    "Tests passed."

part1 input =
    let 
        output = 
            eval input 0 [1] []
    in
        assert (all (== 0) (tail output))
        head output

part2 input =
    head $ eval input 0 [5] []

parseInput input =
    let 
        readInt str =
            read str :: Int
    in
        fromList $ map readInt (splitOn "," input)

main = do
    putStrLn test
    input <- 
        readFile "./inputs/day5.txt"
    putStrLn ("Day 5 -- Part 1: " <> ((show . part1 . parseInput) input))
    putStrLn ("Day 5 -- Part 2: " <> ((show . part2 . parseInput) input))