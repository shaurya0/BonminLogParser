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


testStr1 :: B.ByteString
testStr1 = "Cbc0001I Search completed - best objective -1296.120707310983, took 70350 iterations and 2703 nodes (39.19 seconds)"

getLastNLines :: B.ByteString -> Int -> [B.ByteString]
getLastNLines contents n =
    let lineContents = reverse $ B.lines contents
        lastNLines = Prelude.take n lineContents
    in lastNLines

main :: IO()
main = do
    contents <- B.readFile filePath2
    let lastLines = reverse $ getLastNLines contents 10
    mapM_ B.putStrLn lastLines
    let r =  map (parseOnly bonminResultsParser) lastLines
    let u = rights r
    putStrLn $ show u

