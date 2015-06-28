{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}

module BonminLogParserCmdLine
(
    optionHandler,
    getOpts,
) where

import BonminLogParser
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as C
import Data.Either
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Exit
import Control.Monad (unless, when)


data MyOptions = MyOptions {inputFilePath :: FilePath, outputFilePath :: FilePath} deriving (Data, Typeable, Show, Eq)

myProgOpts :: MyOptions
myProgOpts = MyOptions
    { inputFilePath = def &= typFile &= help "path to inputfile"
    , outputFilePath = def &= typFile &= help "path to outputfile"
    }


getOpts :: IO MyOptions
getOpts  = cmdArgs $ myProgOpts
    &= summary _PROGRAM_INFO
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME :: String
_PROGRAM_NAME = "BonminLogParser"
_PROGRAM_VERSION :: String
_PROGRAM_VERSION = "0.1"
_PROGRAM_INFO :: String
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT :: String
_PROGRAM_ABOUT = "Parses a Bonmin log output generated by e.g ./bonmin some_minlp.nl > log_some_minlp.txt"

getLastNLines :: B.ByteString -> Int -> [B.ByteString]
getLastNLines contents n =
    let lineContents = reverse $ B.lines contents
        lastNLines = Prelude.take n lineContents
    in lastNLines


optionHandler :: MyOptions -> IO ()
optionHandler opts@MyOptions{..}  = do
    inputFileExists <- doesFileExist inputFilePath
    unless inputFileExists $ putStrLn ("the file " ++ inputFilePath ++  ": does not exist") >> exitWith (ExitFailure 1)
    let dir = takeDirectory outputFilePath
    dirGood <- doesDirectoryExist dir
    unless dirGood $ putStrLn ("the directory of the outputfile " ++ outputFilePath ++  ": does not exist") >> exitWith (ExitFailure 1)
    results_ <- parseBonminLog inputFilePath
    let results = [results_]
    outputFileExists <- doesFileExist outputFilePath
    let csvOutput = if outputFileExists then C.encode results else C.encodeDefaultOrderedByName results
    B.appendFile outputFilePath $ LBS.toStrict csvOutput

parseBonminLog :: FilePath -> IO (BonminResults)
parseBonminLog filePath = do
    contents <- B.readFile filePath
    let lastLines = reverse $ getLastNLines contents 10
    let minlpName = (dropExtension . takeFileName) filePath
    let results =  map (parseOnly (bonminResultsParser minlpName)) lastLines
    let retval = map (parseOnly bonminSolverReturnParser) lastLines
    let parsedResult = rights results
    let parsedReturnValue = rights retval
    when (null parsedResult || null parsedReturnValue) $ putStrLn ("Failed to parse : " ++ filePath) >> exitWith (ExitFailure 1)
    let finalResults = (head parsedResult) {solverReturn = head parsedReturnValue }
    return finalResults
