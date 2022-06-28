module Main where

import qualified Parser
import qualified TypeChecker
import qualified Intermediary
import qualified TypeAnnotations
import qualified Text.Parsec.Error as E
import System.IO (IOMode(WriteMode), openFile)
import System.Process (callCommand)
import System.Environment (getArgs)
import Text.Parsec (ParseError)

main = do
    args <- getArgs
    let fn = head args
    inp <- readFile fn
    case (do
        ast <- case Parser.run fn Parser.parse inp of
            Left e -> Left $ print e
            Right a -> return a
        case TypeChecker.typecheck inp ast of
            Left e -> Left $ print ast >> putStrLn e
            Right a -> return a
        let (annotatedAST, typesList) = TypeAnnotations.annotate ast
        let intermediary = Intermediary.encode annotatedAST typesList
        return $ do
            writeFile "out.n" intermediary
            callCommand "./bin/main out.n") of
            Left e -> e
            Right a -> a