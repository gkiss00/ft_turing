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
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics

-- Data Type

data Step = Step {
    read :: String,
    to_state :: String,
    write :: String,
    action :: String
}

data Transition = Transition {
    step1 :: [Step],
    step2 :: [Step],
    step3 :: [Step],
    step4 :: [Step]
}

data Config = Config {
    name :: String,
    alphabet :: [String],
    blank :: String,
    states :: [String],
    initial :: String,
    finals :: [String]
} deriving (Show, Generic)

instance FromJSON Config


main = do
    jsonFile <- getJsonFile
    input <- B.readFile jsonFile
    let mm = decode input :: Maybe Config
    case mm of
        Nothing -> print "Error in config"
        Just m -> outputConfig m

getJsonFile = do
    args <- getArgs
    return $ head args

outputConfig :: Config-> IO ()
outputConfig (Config n a b s i f) = do
    putStrLn ("name:     " ++ (show n))
    putStrLn ("alphabet: " ++ (show a))
    putStrLn ("blank:    " ++ (show b))
    putStrLn ("states:   " ++ (show s))
    putStrLn ("initial:  " ++ (show i))
    putStrLn ("finals:   " ++ (show f))

greet m = (show.name) m ++ "\n" ++ (show.alphabet) m

