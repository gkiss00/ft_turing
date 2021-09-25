-- Comments --

-- Int (bounded)
-- Integer (unbounded)
-- Float & Double
-- Char
-- Bool (True | False)
-- Tuple (K, V)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- Import
import System.Environment
import Data.Map
import Data.List
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics

-- Data Type

data Step = Step {
    read :: String,
    write :: String,
    action :: String
} deriving (Show, Generic)

data Transition = Transition {
    steps :: Map String Step,
    to_state :: String
} deriving (Show, Generic)

data Config = Config {
    name :: String,
    alphabet :: [String],
    blank :: String,
    nb_tapes :: Int,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: Map String [Transition]
} deriving (Show, Generic)

instance FromJSON Config
instance FromJSON Transition
instance FromJSON Step

-- CONST

-- Main


main = do
    jsonFile <- getArg 0
    if jsonFile == "--help" || jsonFile == "-h"
        then outputUsage
        else launchMachine

getArg :: Int -> IO String
getArg i = do
    args <- getArgs
    return $ args !! i

outputUsage :: IO ()
outputUsage = do
    putStrLn "usage: ft_turing [-h / --help] <json file> <input>"
    putStrLn ""
    putStrLn "positional arguments:"
    putStrLn "\t<json file>\t\tjson description of the machine"
    putStrLn "\t<input>\t\tjson input of the machine"
    putStrLn "positional arguments:"
    putStrLn "\t<-h / --help>\t\tshow this help message and exit"

outputConfig :: Config -> IO ()
outputConfig (Config name alphabet blank nb_tapes states initial finals transitions) = do
    putStrLn "****************************************"
    putStrLn ""
    putStrLn ("          " ++ name)
    putStrLn ""
    putStrLn "****************************************"
    putStrLn ("alphabet: " ++ show alphabet)
    putStrLn ("states:   " ++ show nb_tapes)
    putStrLn ("states:   " ++ show states)
    putStrLn ("initial:  " ++ initial)
    putStrLn ("finals:   " ++ show finals)
    -- outputTransitions states transitions 0 (length states)

-- outputTransitions :: [String] -> Map String [Transition] -> Int -> Int -> IO ()
-- outputTransitions list map i s = do
--     if i < s
--         then do
--             putStrLn (list !! i)
--             if i < size map
--                 then do
--                     outputSteps (map ! (list !! i)) 0 (length (map ! (list !! i)))
--                 else putStrLn ""
--             outputTransitions list map (i + 1) s
--         else putStrLn ""

-- outputSteps :: [Step] -> Int -> Int -> IO ()
-- outputSteps list i size = do
--     if i < size
--         then do
--             putStrLn ("\t" ++ (show (list !! i)))
--             outputSteps list (i + 1) size
--         else putStrLn ""

launchMachine :: IO ()
launchMachine = do
    jsonFile <- getArg 0
    json <- B.readFile jsonFile
    input <- getArg 1
    let mm = decode json :: Maybe Config
    case mm of
        Nothing -> print "Error in config"
        Just m -> do
            outputConfig m
            start m input

start :: Config -> String -> IO ()
start config input = do
    let indexes = initIndexes config
    let tapes = initTapes config input
    let currentState = initial config
    run config currentState tapes  indexes

run :: Config ->  String -> [String] -> [Int] -> IO ()
run config currentState tapes  heads = do
    -- if current state == final state exit
    if currentState `elem` finals config
        then do
            putStrLn "Result"
            outputTapes tapes heads 0
        else do
            outputTapes tapes heads 0
            -- get current transition
            let transition = getNextTransition config currentState tapes heads 0
            --  modify tapes
            let new_tapes = modifyTapes tapes heads transition 0
            -- move heads
            let new_heads = modifyIndexes config heads transition 0
            -- set next state
            let new_state = to_state transition
            --putStrLn new_tape
            run config new_state new_tapes  new_heads

increment :: Int -> Int
increment x = x + 1

decrement :: Int -> Int
decrement x = x - 1

initTapes :: Config -> String -> [String]
initTapes config input = replicate 1 (fullyTape config input) ++ replicate (nb_tapes config - 1) (emptyTape config input)

initIndexes :: Config -> [Int]
initIndexes config = replicate (nb_tapes config) 1

duplicate :: String -> Int -> String
duplicate str n = concat $ replicate n str

emptyTape :: Config -> String -> String
emptyTape config str = blank config ++ duplicate (blank config) (length str) ++ duplicate (blank config) 10

fullyTape :: Config -> String -> String
fullyTape config str = blank config ++ str ++ duplicate (blank config) 10

getNextTransition :: Config -> String-> [String] -> [Int] -> Int-> Transition
getNextTransition config currentState tapes indexes i = if test ((transitions config ! currentState) !! i) tapes indexes 0 (nb_tapes config)
    then (transitions config ! currentState) !! i
    else getNextTransition config currentState tapes indexes (i + 1)

--isGoodTransition :: Transition -> [String] -> [Int] -> Int -> Int -> Bool
--isGoodTransition transition tapes indexes i max = if (i == max) || ((Main.read (steps transition ! ("tape" ++ (i + 1)))) == ((tapes !! i) !! (indexes !! i)) && (isGoodTransition transition tapes indexes (i + 1) max))

test :: Transition -> [String] -> [Int] -> Int -> Int -> Bool
test transition tapes indexes i max = (i == max) || (Main.read (steps transition ! ("tape" ++ show (i + 1))) == replicate 1 ((tapes !! i) !! (indexes !! i)) && test transition tapes indexes (i + 1) max)


modifyTapes :: [String] -> [Int] -> Transition -> Int -> [String]
modifyTapes tapes indexes transition i = if i == length tapes
    then replicate 0 ""
    else replicate 1 (Data.List.take (indexes !! i) (tapes !! i) ++ write (steps transition ! ("tape" ++ show (i + 1))) ++ Data.List.drop ((indexes !! i) + 1) (tapes !! i)) ++ modifyTapes tapes indexes transition (i + 1)

modifyIndexes :: Config -> [Int] -> Transition -> Int -> [Int]
modifyIndexes config indexes transition i = if i == nb_tapes config
    then replicate 0 1
    else replicate 1 (updateIndex (indexes !! i) (action (steps transition ! ("tape" ++ show (i + 1))))) ++ modifyIndexes config indexes transition (i + 1)

updateIndex :: Int -> String -> Int
updateIndex i "RIGHT" = i + 1
updateIndex i "LEFT" = i - 1
updateIndex i _ = i

outputTapes :: [String] -> [Int] -> Int -> IO ()
outputTapes tapes indexes i = do
    if i < length tapes
        then do
            putStrLn ("[" ++ Data.List.take (indexes !! i) (tapes !! i) ++ "<" ++ replicate 1 ((tapes !! i) !! (indexes !! i)) ++">" ++ Data.List.drop ((indexes !! i) + 1) (tapes !! i) ++ "]")
            outputTapes tapes indexes (i + 1)
        else putStrLn ""