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


-- todo: get rid of copy pasting here
instance C.ToField SolverReturn where
    toField SUCCESS = "SUCCESS"
    toField INFEASIBLE = "INFEASIBLE"
    toField CONTINUOUS_UNBOUNDED = "CONTINUOUS_UNBOUNDED"
    toField LIMIT_EXCEEDED = "LIMIT_EXCEEDED"
    toField USER_INTERRUPT = "USER_INTERRUPT"
    toField MINLP_ERROR = "MINLP_ERROR"

instance C.ToRecord SolverReturn
instance C.ToRecord BonminResults
instance C.ToNamedRecord BonminResults
instance C.DefaultOrdered BonminResults


prefixParser :: Parser ()
prefixParser = do
    string "Cbc"
    Data.Attoparsec.ByteString.Char8.take 4
    char 'I'
    return ()




computeGap :: Double -> Double -> Double
computeGap solution bestPossible = 100 * abs (bestPossible - solution) / bestPossible

checkInfeasible :: Double -> Bool
checkInfeasible objective = objective == 1.0e50


objectiveParser :: Parser (Maybe Double, Maybe Double)
objectiveParser =
     string "Search completed - best objective" >> space >>
        do
         objective <- double
         return (Just objective, Just 0.0)
    <|> (string "Partial search - best objective" >> space >>
        do
         objective <- double
         let infeasible = checkInfeasible objective
         takeTill isDigit
         bestPossible <- double
         if infeasible then return (Nothing, Nothing) else return (Just objective, Just $ computeGap objective bestPossible) )




iterationParser :: Parser Word
iterationParser = do
    takeTill isDigit
    decimal

nodeParser :: Parser Word
nodeParser = do
    takeTill isDigit
    decimal

timeParser :: Parser Double
timeParser = do
    takeTill isDigit
    double


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
bonminSolverReturnParser =
    (string "bonmin: Optimal" >> return SUCCESS)
    <|> (string "bonmin: Infeasible problem" >> return INFEASIBLE)
    <|> (string "bonmin Continuous relaxation is unbounded." >> return CONTINUOUS_UNBOUNDED)
    <|> (string "bonmin: Optimization interrupted on limit." >> return LIMIT_EXCEEDED)
    <|> (string "bonmin: Optimization interupted on limit." >> return LIMIT_EXCEEDED)
    <|> (string "bonmin: Optimization interrupted by user." >> return USER_INTERRUPT)
    <|> (string "bonmin: Optimization interupted by user." >> return USER_INTERRUPT)
    <|> (string "bonmin: Error encountered in optimization." >> return MINLP_ERROR)
