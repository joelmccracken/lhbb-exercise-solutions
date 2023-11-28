{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Lib (module Lib) where

import RIO hiding (concat)

import Prelude (putStrLn)

import qualified RIO.HashMap as HM
import qualified RIO.List as List

import Options.Applicative
import Data.Text hiding (replicate, find, any, filter, foldl')
import Data.List (find)
import Data.Char (isLetter)

defaultNeedles :: Text
defaultNeedles = "blue,green,yellow,orange,red,purple"

data KwdArgs = KwdArgs { haystack :: Text
                       , needles :: Text
                       }

data Results
  = Results
  { hits :: HashMap Text Int
  , other :: Int
  }
  deriving (Eq, Show)

splitNeedles :: Text -> [Text]
splitNeedles = split (== ',')

haystackArgumentParser :: Parser Text
haystackArgumentParser = strArgument (metavar "HAYSTACK")

needlesArgumentParser :: Parser Text
needlesArgumentParser = pack <$> (option str (metavar "NEEDLES" <> long "needles" <> value (unpack defaultNeedles)))

kwdArgsParser :: Parser KwdArgs
kwdArgsParser = KwdArgs <$> haystackArgumentParser <*> needlesArgumentParser

parserInfo :: ParserInfo KwdArgs
parserInfo = info (kwdArgsParser <**> helper)
     (  fullDesc
     <> progDesc "Searches for favorite colors in HAYSTACK"
     <> header "kwd - your friendly neighborhood color searcher"
     )

inHaystack :: [Text] -> Text -> Bool
inHaystack haystack' needle = any (needle ==) haystack'

prepareHaystack :: Text -> [Text]
prepareHaystack =
    filter (not . (== mempty)) -- remove empty strings
  . split (not . isLetter) -- split strings when non-letter-chracters are encountered
  . toLower -- lowercase entire haystack (normalizing cases)

searchHaystack :: [Text] -> [Text] -> Results -> Results
searchHaystack needles' haystack' results =
  let
    examineWord :: Results -> Text -> Results
    examineWord results' word =
      let
        res' =  find (== word) needles'
      in
        case res' of
          Nothing -> results' { other = results'.other + 1 }
          Just needle ->
            results' { hits = HM.insertWith (+) needle 1 results'.hits }
  in
    foldl' examineWord results haystack'

-- wraps the entirety of the business logic
-- extracted from the needs of being a CLI tool
-- for testing
seriousBusiness :: Text -> Text -> Results
seriousBusiness haystack' needlepile =
  let
    results = Results HM.empty 0
    hs' = prepareHaystack haystack'
    needles' = toLower <$> (splitNeedles needlepile)
    _searchResults = find (inHaystack hs') needles'
    searchResults = searchHaystack needles' hs' results
  in
    searchResults

displayResults :: Results -> Text
displayResults results =
 let
   displayNeedleHits :: Text -> Int -> Text
   displayNeedleHits needle hits' = needle <> ": " <> tshow hits' <> "\n"
   hitsList = results.hits & HM.toList & List.sortOn (Down . snd)
   rendered = hitsList <&> uncurry displayNeedleHits & concat
 in
   "Found keywords: \n" <> rendered <> "\nNon-keyword count: " <> tshow results.other

libMain :: IO ()
libMain = do
  KwdArgs hs argNeedles <- execParser parserInfo
  let results = seriousBusiness hs argNeedles
  displayResults results & unpack & putStrLn
