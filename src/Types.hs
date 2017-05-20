{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Data.Function (on)
import           Data.List     (sortBy)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Text     (Text)
import qualified Data.Text     as T

data Transcript = Transcript
  { content  :: Text
  , filepath :: FilePath
  } deriving (Show, Eq)

-- | Represents a fragment of text from a transcription.
data Fragment = Fragment
  { code     :: Text
  , text     :: Text
  , location :: Location
  } deriving (Show, Eq)

-- | Represents a location in a transcription.
data Location = Location
  { fileName   :: Text
  , lineNumber :: (Integer, Integer)
  } deriving (Show, Eq, Ord)

isSingleLine :: Location -> Bool
isSingleLine Location {lineNumber = (s, e)} = s == e

-- | Represents one set of coded text, indexed by code.
newtype CodedText = CodedText
  { unCodedText :: Map Text [Fragment]
  } deriving (Show)

instance Monoid CodedText where
  mempty = CodedText Map.empty
  CodedText ct1 `mappend` CodedText ct2 =
    CodedText $ Map.unionWith (++) ct1 ct2

-- | Add a fragment into a CodedText value.
addFragment :: CodedText -> Fragment -> CodedText
addFragment CodedText {unCodedText = map} fragment =
  CodedText $ Map.insertWith (++) (code fragment) [fragment] map

-- | Add multiple fragments into a CodedText value.
addFragments :: CodedText -> [Fragment] -> CodedText
addFragments = foldl addFragment

-- | Create a coded text from a list of fragments.
makeCodedText :: [Fragment] -> CodedText
makeCodedText = addFragments $ CodedText Map.empty

ppCodedText :: CodedText -> Text
ppCodedText CodedText { unCodedText = codedText } =
  Map.foldWithKey printEntry "" codedText
  where
    printEntry k v a =
      T.concat [ "## "
               , k
               , "\n\n"
               , T.concat . fmap ppFragment . sortBy (compare `on` location) $ v
               , "---\n\n"
               , a
               ]

ppFragment :: Fragment -> Text
ppFragment fragment = T.concat
  [ ppLocation . location $ fragment
  , "\n\n"
  , text fragment
  , "\n\n"
  ]

ppLocation :: Location -> Text
ppLocation location = T.concat
  [ "  * File: "
  , fileName location
  , " "
  , ppLineNumber . lineNumber $ location
  ]

ppLineNumber :: (Integer, Integer) -> Text
ppLineNumber (a, b) = T.concat ["(line ", lineNum (a, b), ")"]
  where
    showInt = T.pack . show
    lineNum (a, b) =
      if a == b
        then showInt a
        else T.concat [showInt a, "-", showInt b]
