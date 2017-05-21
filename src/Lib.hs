{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( writeCodes
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Reader
import Parser
import Types

writeCodes :: FilePath -> FilePath -> IO ()
writeCodes transcriptsPath outputPath = do
  transcripts <- findTranscriptions transcriptsPath
  let result = mapM parseTranscript transcripts
      filenames = fmap filepath transcripts
  case result of
    Right cts -> do
      let output =
            T.unlines
              [ "Source files:\n"
              , T.unlines $ fmap (T.append "  * " . T.pack) filenames
              , ppCodedText . mconcat $ cts
              ]
      T.writeFile outputPath output
    Left err -> print err
