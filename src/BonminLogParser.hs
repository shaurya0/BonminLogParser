{-# LANGUAGE OverloadedStrings #-}

module BonminLogParser where

import Data.Attoparsec.ByteString.Char8
import Data.Word
import Control.Applicative


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


prefixParser :: Parser ()
prefixParser = do
    string "Cbc"
    Data.Attoparsec.ByteString.Char8.take 4
    char 'I'
    return ()




computeGap :: Double -> Double -> Double
computeGap solution bestPossible = 100 * abs (bestPossible - solution) / bestPossible

checkInfeasible :: Double -> Bool
checkInfeasible objective =
    if objective == 1.0e50
    then True else False


objectiveParser :: Parser (Maybe Double, Maybe Double)
objectiveParser = do
    ( string "Search completed - best objective" >> space >>
        do
         objective <- double
         return (Just objective, Just 0.0) )
    <|> (string "Partial search - best objective" >> space >>
        do
         objective <- double
         let infeasible = checkInfeasible objective
         takeTill (\c -> (isDigit c))
         bestPossible <- double
         if infeasible then return (Nothing, Nothing) else return (Just objective, Just $ computeGap objective bestPossible) )




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


bonminResultsParser :: Parser BonminResults
bonminResultsParser = do
    prefixParser
    space
    (objective, bestPossible) <- objectiveParser
    iters <- iterationParser
    numNodes <- nodeParser
    solutionTime <- timeParser
    return $ BonminResults iters numNodes solutionTime objective bestPossible SUCCESS


bonminSolverReturnParser :: Parser SolverReturn
bonminSolverReturnParser = do
    (string "bonmin: Optimal" >> return SUCCESS)
    <|> (string "bonmin: Infeasible problem" >> return INFEASIBLE)
    <|> (string "bonmin Continuous relaxation is unbounded." >> return CONTINUOUS_UNBOUNDED)
    <|> (string "bonmin: Optimization interrupted on limit." >> return LIMIT_EXCEEDED)
    <|> (string "bonmin: Optimization interupted on limit." >> return LIMIT_EXCEEDED)
    <|> (string "bonmin: Optimization interrupted by user." >> return USER_INTERRUPT)
    <|> (string "bonmin: Optimization interupted by user." >> return USER_INTERRUPT)
    <|> (string "bonmin: Error encountered in optimization." >> return MINLP_ERROR)

bonminLogParser :: Parser [BonminResults]
bonminLogParser = do
    many bonminResultsParser <* endOfLine
