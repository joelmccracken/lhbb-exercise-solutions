{-# LANGUAGE OverloadedStrings #-}

module Lib (module Lib) where

import Options.Applicative
import Data.Text hiding (replicate, find, any, filter)
import Data.Maybe
import Data.List (find)
import Data.Char (isLetter)

defaultNeedles :: Text
defaultNeedles = "blue,green,yellow,orange,red,purple"

data KwdArgs = KwdArgs { haystack :: Text
                       , needles :: Text
                       }

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

-- wraps the entirety of the business logic
-- extracted from the needs of being a CLI tool
-- for testing
seriousBusiness :: Text -> Text -> Bool
seriousBusiness haystack' needlepile =
  let
    hs' = prepareHaystack haystack'
    needles' = toLower <$> (splitNeedles needlepile)
    searchResults = find (inHaystack hs') needles'
  in
    isJust searchResults

libMain :: IO ()
libMain = do
  KwdArgs hs argNeedles <- execParser parserInfo
  if seriousBusiness hs argNeedles then
    putStrLn "At least one keyword found!"
  else
    putStrLn "No keywords found."
