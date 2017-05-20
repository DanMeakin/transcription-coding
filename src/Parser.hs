{-# LANGUAGE OverloadedStrings #-}
module Parser where

import           Data.Function    (on)
import           Data.List        (groupBy, sortBy)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           Text.Parsec
import           Text.Parsec.Text

import           Types

newLocation :: FilePath -> Location
newLocation path = Location {fileName = T.pack path, lineNumber = (1, 1)}

fragmentStart = '<'

codeDelim = '{'

fragmentEnd = '}'

parseTranscript :: Transcript -> Either ParseError CodedText
parseTranscript transcript =
  runParser codeText (newLocation . filepath $ transcript) "" . content $ transcript

incrLineNumber :: Location -> Location
incrLineNumber loc =
  let (n, _) = lineNumber loc
  in loc { lineNumber = (succ n, succ n) }

codeText :: Parsec Text Location CodedText
codeText = makeCodedText <$> (nonCodedSection *> endBy codedSection nonCodedSection)
  where
    nonCodedSection = many (incrNewline <|> satisfy (/= fragmentStart)) *> pure () <|> eof
    grouped = groupBy ((==) `on` code) . sortBy (compare `on` T.toLower . code)
    listHead []    = []
    listHead (x:_) = x

codedSection :: Parsec Text Location Fragment
codedSection = do
  manyTill anyCharIncrNewline $ char fragmentStart
  startState <- getState
  thisCode <- manyTill anyCharIncrNewline $ char codeDelim
  thisText <- manyTill anyCharIncrNewline $ char fragmentEnd
  endState <- getState
  let location = startState { lineNumber = ( fst . lineNumber $ startState
                                           , fst . lineNumber $ endState
                                           )
                            }
  return Fragment {code = T.pack thisCode, text = T.pack thisText, location = location }

anyCharIncrNewline :: Parsec Text Location Char
anyCharIncrNewline = incrNewline <|> anyChar

incrNewline :: Parsec Text Location Char
incrNewline = do
  char '\n'
  state <- getState
  putState $ incrLineNumber state
  return '\n'
