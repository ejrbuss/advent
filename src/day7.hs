import Control.Exception
import Data.Ord
import Data.List
import Data.List.Split
import qualified Data.Sequence as Seq

data Computation = 
    Output [Int] |
    Continuation [Int] ([Int] -> Computation)

data Mode = 
    Position | Immediate
    deriving (Eq, Show)

data Op =
    Add | Mul | In | Out | Jt | Jf | Lt | Eq | Exit
    deriving (Eq, Show)

type Instruction =
    (Op, Mode, Mode)

type Intcode =
    Seq.Seq Int

outputOf :: Computation -> [Int]
outputOf (Output output) =
    output
outputOf (Continuation output _) =
    output

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

load :: Intcode -> [Int] -> Computation
load memory input =
    eval memory 0 [] input

eval :: Intcode -> Int -> [Int] -> [Int] -> Computation
eval memory pc output input =
    let 
        param Immediate offset =
            Seq.index memory (pc + offset)
        param Position offset =
            Seq.index memory (param Immediate offset)

        write offset value =
            (Seq.update (param Immediate offset) value memory)

        -- Adds the values of its firsta nd second parameters and stores them
        -- at its 3rc parameter
        opcode (Add, mode1, mode2) = 
            eval 
                (write 3 (param mode1 1 + param mode2 2))
                (pc + 4)
                output
                input

        -- Multiplies the values of its firsta nd second parameters and stores 
        -- them at its 3rc parameter
        opcode (Mul, mode1, mode2) =
            eval
                (write 3 (param mode1 1 * param mode2 2))
                (pc + 4)
                output
                input

        -- Takes a single integer as input and saves it to the position given
        -- by its only parameter
        opcode (In, _, _) =
            case input of
                [] -> 
                    Continuation output (eval memory pc [])
                (i:is) ->
                    eval
                        (write 1 i)
                        (pc + 2)
                        output
                        is

        -- Ouputs the value of its only parameter
        opcode (Out, mode1, _) =
            eval
                memory
                (pc + 2)
                ((param mode1 1) : output)
                input

        -- If the first parameter is non-zero, it sets pc to the value of the
        -- second parameter, otherwise it does nothing
        opcode (Jt, mode1, mode2) =
            eval
                memory
                (if (param mode1 1) /= 0
                    then (param mode2 2) 
                    else pc + 3)
                output
                input

        -- If the first parameter is zero, it sets pc to the value of the
        -- second parameter, otherwise it does nothing
        opcode (Jf, mode1, mode2) =
            eval
                memory
                (if (param mode1 1) == 0 
                    then (param mode2 2) 
                    else pc + 3)
                output
                input

        -- If the first parameter is less than the second, store 1 in the 
        -- position given by its third parameter, otherwise store 0
        opcode (Lt, mode1, mode2) =
            eval
                (if (param mode1 1) < (param mode2 2) 
                    then (write 3 1)
                    else (write 3 0))
                (pc + 4)
                output
                input

        -- If the first parameter is equal to the second, store 1 in the 
        -- position given by its third parameter, otherwise store 0
        opcode (Eq, mode1, mode2) =
            eval
                (if (param mode1 1) == (param mode2 2) 
                    then (write 3 1)
                    else (write 3 0))
                (pc + 4)
                output
                input

        -- Exits the program and returns the output
        opcode (Exit, _, _) =
            Output output
    in
        opcode $ decode $ param Immediate 0

amplify :: [Int] -> [Int] -> Intcode -> [Int]
amplify [] signal _ =
    signal
amplify (phase:phases) signal intcode =
    amplify phases (outputOf $ load intcode (phase : signal)) intcode

maxPhase :: Int -> [Int] -> Intcode -> [Int]
maxPhase amplifiers signal intcode =
    let
        signalOf phases =
            head $ amplify phases signal intcode
    in
        maximumBy (comparing signalOf) (phaseGenerator amplifiers)

phaseGenerator :: Int -> [[Int]]
phaseGenerator n = 
    permutations [0..(n - 1)]

amplifier :: Intcode -> Int -> Computation
amplifier memory phase =
    load memory [phase]

feedbackLoop :: [Int] -> [Int] -> Intcode -> [Int]
feedbackLoop phases signal intcode =
    let 
        -- Base case
        loop _ (Output output : []) _ =
            output
        -- Reamp, reload amps
        loop signal [] reamp = 
            loop signal (reverse reamp) []
        -- Bottomed out, forward output rather than signal
        loop signal (Output output : amps) reamp =
            loop output amps reamp
        -- Common case, perfform continuation and forward output
        loop signal (Continuation output continuation : amps) reamp =
            loop (outputOf amp) amps (amp : reamp)
            where 
                amp = 
                    continuation signal
    in
        loop signal (map (amplifier intcode) phases) []



maxFeedbackPhase :: Int -> [Int] -> Intcode -> [Int]
maxFeedbackPhase amplifiers signal intcode =
    let
        signalOf phases =
            head $ feedbackLoop phases signal intcode
    in
        maximumBy 
            (comparing signalOf) 
            (map (map (+ 5)) $ phaseGenerator amplifiers)

test =
    assert ([43210] == amplify [4,3,2,1,0] [0] (parseInput 
        "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
    assert ([54321] == amplify [0,1,2,3,4] [0] (parseInput 
            "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"))
    assert ([65210] == amplify [1,0,4,3,2] [0] (parseInput 
        "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))
    assert ([4,3,2,1,0] == maxPhase 5 [0] (parseInput 
        "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
    assert ([0,1,2,3,4] == maxPhase 5 [0] (parseInput 
        "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"))
    assert ([1,0,4,3,2] == maxPhase 5 [0] (parseInput 
        "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))
    assert ([139629729] == feedbackLoop [9,8,7,6,5] [0] (parseInput
        "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))
    assert ([18216] == feedbackLoop [9,7,8,5,6] [0] (parseInput
        "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"))
    assert ([9,8,7,6,5] == maxFeedbackPhase 5 [0] (parseInput
        "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))
    assert ([9,7,8,5,6] == maxFeedbackPhase 5 [0] (parseInput
        "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"))
    "Tests passed."

part1 input =
    head $ amplify (maxPhase 5 [0] input) [0] input

part2 input =
    head $ feedbackLoop (maxFeedbackPhase 5 [0] input) [0] input

parseInput input =
    let 
        readInt str =
            read str :: Int
    in
        Seq.fromList $ map readInt (splitOn "," input)

main = do
    putStrLn test
    input <- 
        readFile "./inputs/day7.txt"
    putStrLn ("Day 7 -- Part 1: " <> ((show . part1 . parseInput) input))
    putStrLn ("Day 7 -- Part 2: " <> ((show . part2 . parseInput) input))