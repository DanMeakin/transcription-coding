module Reader where

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory.Tree
import System.FilePath.Posix

import Types

findTranscriptions :: FilePath -> IO [Transcript]
findTranscriptions path = takeTranscriptions . dirTree <$> readDirectory path

takeTranscriptions :: DirTree String -> [Transcript]
takeTranscriptions Dir {contents = files} = concatMap takeTranscriptions files
takeTranscriptions File {name = name, file = file} =
  [ Transcript {content = T.pack file, filepath = name}
  | takeExtension name == ".markdown"
  ]
