module Main where

import Control.Monad (guard, unless)
import Data.Text.IO (getLine, readFile)
import Parser
import Scanner
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Prelude hiding (getLine, readFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> runFile f
        [] -> hSetBuffering stdout NoBuffering >> runPrompt
        _ -> putStrLn "Usage: mpl FILE, mpl" >> exitFailure

handleFile :: FilePath -> Bool
handleFile f =
    let ext = dropWhile (/= '.') f
     in ext == ".kron"

runFile :: FilePath -> IO ()
runFile f = do
    guard $ handleFile f
    _bytes <- readFile f
    undefined

runPrompt :: IO ()
runPrompt = loop
  where
    loop = do
        putStr "-> "
        l <- getLine
        unless (l == "quit") $ do
            let (_, s) = runScan (initScanner l) scanTokens
            let p = runParser parse (initParser (sTokens s))
            case p of
                Left _ -> putStrLn "error Parsing" >> exitFailure
                Right e -> print e >> loop
        exitSuccess
