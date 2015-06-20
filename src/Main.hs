{-# LANGUAGE OverloadedStrings #-}
module Main where

import BonminLogParser
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import System.IO
import Control.Applicative
import Data.Either

filePath1 :: FilePath
filePath1 = "/home/shaurya/Development/BonminLogParser/data/sample_log.txt"

filePath2 :: FilePath
filePath2 = "/home/shaurya/Development/BonminLogParser/data/sample_log2.txt"

filePath3 :: FilePath
filePath3 = "/home/shaurya/Development/BonminLogParser/data/sample_log3.txt"



getLastNLines :: B.ByteString -> Int -> [B.ByteString]
getLastNLines contents n =
    let lineContents = reverse $ B.lines contents
        lastNLines = Prelude.take n lineContents
    in lastNLines

main :: IO()
main = do
    contents <- B.readFile filePath3
    let lastLines = reverse $ getLastNLines contents 10
    mapM_ B.putStrLn lastLines
    let results =  map (parseOnly bonminResultsParser) lastLines
    let retval = map (parseOnly bonminSolverReturnParser) lastLines
    let u = head $ rights results
    let t = head $ rights retval
    let v = u {solverReturn =  t}
    putStrLn $ show v

