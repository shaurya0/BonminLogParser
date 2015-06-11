{-# LANGUAGE OverloadedStrings #-}
module Main where

import BonminLogParser
import Data.Attoparsec.Char8
import qualified Data.ByteString as B
import System.IO
import Control.Applicative

filePath :: FilePath
filePath = "/home/shaurya/Development/BonminLogParser/data/sample_log.txt"


testStr1 :: B.ByteString
testStr1 = "Cbc0001I Search completed - best objective -1296.120707310983, took 70350 iterations and 2703 nodes (39.19 seconds)"


main :: IO()
main = do
    B.putStrLn "hello world"
    -- withFile filePath ReadMode (\handle -> do
    --     contents <- hGetContents handle
    --     putStr contents)

