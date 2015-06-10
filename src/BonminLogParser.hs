{-# LANGUAGE OverloadedStrings #-}

module BonminLogParser where

import Data.Attoparsec.Char8
import Data.Word
import qualified Data.ByteString as B
import Control.Applicative

        -- enum SolverReturn{
        --     SUCCESS,
        --     INFEASIBLE,
        --     CONTINUOUS_UNBOUNDED,
        --     LIMIT_EXCEEDED,
        --     USER_INTERRUPT,
        --     MINLP_ERROR};


data SolverReturn = SUCCESS
                | INFEASIBLE
                | CONTINUOUS_UNBOUNDED
                | LIMIT_EXCEEDED
                | USER_INTERRUPT
                | MINLP_ERROR
                deriving (Show)



data BonminResults =
    BonminResults {   nodes :: Word
                    , iterations :: Word
                    , time :: Double
                    , bestObjective :: Maybe Double
                    , gap :: Maybe Double
                    , solverReturn :: SolverReturn
                    } deriving (Show)




-- parseLog :: FilePath -> BonminResults
-- parseLog filePath = error "todo"

prefixParser :: Parser B.ByteString
prefixParser = do
    string "Cbc"
    Data.Attoparsec.Char8.takeWhile isDigit
    char 'I'
    space
    char 't'
    takeByteString


linePrefixParser :: Parser (Double, Double)
linePrefixParser = do
    ( string "Search completed - best objective " >>
        do
         bestObjective <- double
         let gap = 0.0
         return (bestObjective, gap) )
    <|> (string "Partial search - best objective " >>
        do
         bestObjective <- double
         takeTill (\c -> (c == '-') || (isDigit c))
         gap <- double
         return (bestObjective, gap) )




nodeParser :: Parser Int
nodeParser = do
    string "and"
    space
    numNodes <- decimal
    space
    string "nodes"
    return numNodes

