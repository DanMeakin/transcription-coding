module Main where

import           Lib
import           System.Environment (getArgs)

data Args
  = NoArgs
  | Args String
         String

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  case args of
    NoArgs ->
      putStrLn "trans-codes [TRANSCRIPTIONS-DIRECTORY] [OUTPUT-FILE]"

    Args transDir outputFile ->
      writeCodes transDir outputFile

  where
    parseArgs [transDir, outputFile] = Args transDir outputFile
    parseArgs _                      = NoArgs
