{-# LANGUAGE OverloadedStrings #-}

module BonminLogParser where

import Data.Attoparsec.ByteString.Char8
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
    BonminResults {   iterations :: Word
                    , nodes :: Word
                    , time :: Double
                    , bestObjective :: Maybe Double
                    , gap :: Maybe Double
                    , solverReturn :: SolverReturn
                    } deriving (Show)




parseLog :: FilePath -> BonminResults
parseLog filePath = error "todo"

prefixParser :: Parser ()
prefixParser = do
    string "Cbc"
    Data.Attoparsec.ByteString.Char8.take 4
    char 'I'
    return ()


objectiveParser :: Parser (Double, Double)
objectiveParser = do
    ( string "Search completed - best objective" >> space >>
        do
         objective <- double
         let bestPossible = 0.0
         return (objective, bestPossible) )
    <|> (string "Partial search - best objective" >> space >>
        do
         objective <- double
         takeTill (\c -> (isDigit c))
         bestPossible <- double
         return (objective, bestPossible) )

iterationParser :: Parser Word
iterationParser = do
    takeTill (\c -> (isDigit c))
    numIterations <- decimal
    return numIterations

nodeParser :: Parser Word
nodeParser = do
    takeTill (\c -> (isDigit c))
    numNodes <- decimal
    return numNodes

timeParser :: Parser Double
timeParser = do
    takeTill (\c -> (isDigit c))
    solutionTime <- double
    return solutionTime

checkInfeasible :: (Double, Double) -> (Maybe Double, Maybe Double)
checkInfeasible (x,y) = (Just x, Just y)


bonminResultsParser :: Parser BonminResults
bonminResultsParser = do
    prefixParser
    space
    (objective, bestPossible) <- objectiveParser
    let (x, y) = checkInfeasible (objective, bestPossible)
    iters <- iterationParser
    numNodes <- nodeParser
    solutionTime <- timeParser
    return $ BonminResults iters numNodes solutionTime x y SUCCESS





