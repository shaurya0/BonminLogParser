{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module BonminLogParser
(
    bonminResultsParser,
    bonminSolverReturnParser,
    BonminResults(..),
    SolverReturn(..),
) where

import Data.Attoparsec.ByteString.Char8
import Data.Word
import Control.Applicative
import qualified Data.Csv as C
import qualified Data.Csv.Incremental as CI
import GHC.Generics

data SolverReturn = SUCCESS
                | INFEASIBLE
                | CONTINUOUS_UNBOUNDED
                | LIMIT_EXCEEDED
                | USER_INTERRUPT
                | MINLP_ERROR
                deriving (Show, Generic)



data BonminResults =
    BonminResults {   problemName :: String
                    , iterations :: Word
                    , nodes :: Word
                    , time :: Double
                    , bestObjective :: Maybe Double
                    , gap :: Maybe Double
                    , solverReturn :: SolverReturn
                    } deriving (Show, Generic)

instance C.ToField SolverReturn where
    toField SUCCESS = "SUCCESS"
    toField INFEASIBLE = "INFEASIBLE"
    toField CONTINUOUS_UNBOUNDED = "CONTINUOUS_UNBOUNDED"
    toField LIMIT_EXCEEDED = "LIMIT_EXCEEDED"
    toField USER_INTERRUPT = "USER_INTERRUPT"
    toField MINLP_ERROR = "MINLP_ERROR"

instance C.ToRecord SolverReturn
instance C.ToRecord BonminResults



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


bonminResultsParser :: String -> Parser BonminResults
bonminResultsParser minlpName = do
    prefixParser
    space
    (objective, bestPossible) <- objectiveParser
    iters <- iterationParser
    numNodes <- nodeParser
    solutionTime <- timeParser
    return $ BonminResults minlpName iters numNodes solutionTime objective bestPossible SUCCESS


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
