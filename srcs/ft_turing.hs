-- Comments --

-- Int (bounded)
-- Integer (unbounded)
-- Float & Double
-- Char
-- Bool (True | False)
-- Tuple (K, V)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
    to_state :: String,
    write :: String,
    action :: String
} deriving (Show, Generic)

data Config = Config {
    name :: String,
    alphabet :: [String],
    blank :: String,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: Map String [Step]
} deriving (Show, Generic)

instance FromJSON Config
instance FromJSON Step


main = do
    jsonFile <- getArg 0
    if jsonFile == "--help" || jsonFile == "-h"
        then outputUsage
        else launchMachine

getArg :: Int -> IO String
getArg i = do
    args <- getArgs
    if Data.List.null args
        then return "--help"
        else if i >= length args
            then do
                error "No tape passed to the machine"
                return "--help"
        else return $ args !! i

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
outputConfig (Config name alphabet blank states initial finals transitions) = do
    putStrLn "****************************************"
    putStrLn ""
    putStrLn ("          " ++ name)
    putStrLn ""
    putStrLn "****************************************"
    putStrLn ("alphabet: " ++ show alphabet)
    putStrLn ("states:   " ++ show states)
    putStrLn ("initial:  " ++ initial)
    putStrLn ("finals:   " ++ show finals)
    outputTransitions states transitions 0 (length states)

outputTransitions :: [String] -> Map String [Step] -> Int -> Int -> IO ()
outputTransitions list map i s = do
    if i < s
        then do
            putStrLn (list !! i)
            if i < size map
                then do
                    outputSteps (map ! (list !! i)) 0 (length (map ! (list !! i)))
                else putStrLn ""
            outputTransitions list map (i + 1) s
        else putStrLn ""

outputSteps :: [Step] -> Int -> Int -> IO ()
outputSteps list i size = do
    if i < size
        then do
            putStrLn ("\t" ++ show (list !! i))
            outputSteps list (i + 1) size
        else putStrLn ""

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

duplicate :: String -> Int -> String
duplicate str n = concat $ replicate n str

fullyTape :: Config -> String -> String
fullyTape config str = blank config ++ str ++ duplicate (blank config) 10

start :: Config -> String -> IO ()
start config input = do
    let index = 1
    let tape = fullyTape config input
    let currentState = initial config;
    run config tape currentState index

run :: Config -> String -> String -> Int -> IO ()
run config tape currentState index = do
    -- if current state == final state exit
    if currentState `elem` finals config
        then putStrLn ("Result: " ++ tape)
        else do

            -- get current state
            let state = getStateToApply config currentState -- list of steps
            let step = getStepToApply state [tape !! index] 0 -- step to apply
            case step of
                Just sss ->do
                    outputCurrentTape tape index (write sss)
                    --  modify tape
                    let new_tape = Data.List.take index tape ++ write sss ++ Data.List.drop (index + 1) tape
                    -- move the head
                    let new_index = if action sss == "RIGHT"
                        then increment index
                        else decrement index
                    -- set next state
                    let next_state = to_state sss
                    --putStrLn new_tape
                    run config new_tape next_state new_index
                Nothing -> do
                    putStrLn "Unknown state reached"

outputCurrentTape :: String -> Int -> String -> IO ()
outputCurrentTape tape index step = do
    putStrLn ("[" ++ Data.List.take index tape ++ "<" ++ [tape !! index] ++">" ++ Data.List.drop (index + 1) tape ++ "]")

getStepToApply :: [Step] -> String -> Int -> Maybe Step
getStepToApply steps current index
  | index >= length steps = Nothing
  | Main.read (steps !! index) == current = Just (steps !! index)
  | otherwise = getStepToApply steps current (index + 1)

getStateToApply :: Config -> String -> [Step]
getStateToApply (Config _ _ _ _ _ _ transitions) currentSate = transitions ! currentSate

increment :: Int -> Int
increment x = x + 1

decrement :: Int -> Int
decrement x = x - 1