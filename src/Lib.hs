{-# LANGUAGE OverloadedStrings #-}

module Lib (module Lib) where

import Options.Applicative
import Data.Text hiding (replicate, find, length)
import Data.Maybe
import Data.List (find)

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

sample :: Parser KwdArgs
sample = KwdArgs <$> haystackArgumentParser <*> needlesArgumentParser

parserInfo :: ParserInfo KwdArgs
parserInfo = info (sample <**> helper)
     (  fullDesc
     <> progDesc "Searches for favorite colors in HAYSTACK"
     <> header "kwd - your friendly neighborhood color searcher"
     )

inHaystack :: Text -> Text -> Bool
inHaystack haystack' needle = fst (breakOn needle haystack') /= haystack'

libMain :: IO ()
libMain = do
  KwdArgs hs argNeedles <- execParser parserInfo
  -- let needlesToUse =
  --       if length argNeedles > 0 then
  --         argNeedles
  --       else
  --         splitNeedles defaultNeedles
  let searchResults = find (inHaystack hs) (splitNeedles argNeedles)
  -- putStrLn $ show searchResults
  if isJust searchResults then
    putStrLn "At least one keyword found!"
  else
    putStrLn "No keywords found."
