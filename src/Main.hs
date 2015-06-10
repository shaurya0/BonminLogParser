{-# LANGUAGE OverloadedStrings #-}
module Main where

import BonminLogParser
import Data.Attoparsec.Char8
import System.IO

filePath :: FilePath
filePath = "/home/shaurya/Development/BonminLogParser/data/sample_log.txt"

main :: IO()
main = do
    parseTest prefixParser "Cbc0001I t asdf"
    -- parseTest nodeParser " and 2703 nodes "
    -- withFile filePath ReadMode (\handle -> do
    --     contents <- hGetContents handle
    --     putStr contents)

