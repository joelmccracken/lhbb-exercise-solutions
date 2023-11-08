{-# LANGUAGE OverloadedStrings #-}

module Lib (module Lib) where

import Options.Applicative
import Data.Text hiding (replicate, find, length, any)
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

libMain :: IO ()
libMain = do
  KwdArgs hs argNeedles <- execParser parserInfo
  let hs' = split (not . isLetter) $ toLower hs
  let needles' = toLower <$> (splitNeedles argNeedles)
  let searchResults = find (inHaystack hs') needles'
  if isJust searchResults then
    putStrLn "At least one keyword found!"
  else
    putStrLn "No keywords found."
