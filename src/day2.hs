import Control.Exception
import Data.List.Split
import Data.Sequence
import Data.Maybe

output memory =
    index memory 0

setNoun noun memory =
    update 1 noun memory

setVerb verb memory =
    update 2 verb memory

eval memory pc = 
    let
        reg n = 
            (index memory (pc + n))

        atReg n =
            (index memory (reg n))

        opcode 1 =
            eval (update (reg 3) (atReg 1 + atReg 2) memory) (pc + 4)
        opcode 2 =
            eval (update (reg 3) (atReg 1 * atReg 2) memory) (pc + 4)
        opcode 99 = 
            memory
    in
        opcode (index memory pc)

evalIntcode noun verb intcode  =
    output (eval (setNoun noun (setVerb verb intcode)) 0)

findNounVerb output intcode =
    let
        test noun verb =
            if evalIntcode noun verb intcode == output
                then noun * 100 + verb
                else if noun == 99
                    then test 0 (verb + 1)
                    else test (noun + 1) verb
    in
        test 0 0

test =
    assert (eval 
        (fromList [1,9,10,3,2,3,11,0,99,30,40,50]) 0 == 
        (fromList [3500,9,10,70,2,3,11,0,99,30,40,50]))
    assert (eval
        (fromList [1,0,0,0,99]) 0 ==
        (fromList [2,0,0,0,99]))
    assert (eval
        (fromList [2,3,0,3,99]) 0 ==
        (fromList [2,3,0,6,99]))
    assert (eval
        (fromList [2,4,4,5,99,0]) 0 ==
        (fromList [2,4,4,5,99,9801]))
    assert (eval
        (fromList [1,1,1,4,99,5,6,0,99]) 0 ==
        (fromList [30,1,1,4,2,5,6,0,99]))
    "Tests passed."

part1 =
    evalIntcode 12 2

part2 = 
    findNounVerb 19690720

readInt str =
    read str :: Int

parseInput input =
    fromList $ map readInt (splitOn "," input)

main = do 
    putStrLn test
    input <- 
        readFile "./inputs/day2.txt"
    putStrLn ("Day 2 -- Part 1: " <> ((show . part1 . parseInput) input))
    putStrLn ("Day 2 -- Part 2: " <> ((show . part2 . parseInput) input))