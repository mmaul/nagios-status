{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : NagiosStatus
-- Copyright   : (c) Mike Maul 2013
--
-- License     : BSD3
-- Maintainer  : mike.maul@gmail.com
-- Stability   : experimental
-- Portability : GHC

module NagiosStatus (
                    Section(..),
                    block,
                    readStatus,
                    KV,
                    performanceData,
                    parsePreformanceData,
                    ) where
import Data.Word
import Data.Time
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Control.Applicative hiding ( (<|>) , many)
import Data.Map as Map
import Data.String (lines)
import Data.Either as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Char (isSpace)
import Text.Parsec.String
import System.Exit
import System.Environment
import Data.Char ( toUpper, chr, ord )
import Control.Monad.Trans (liftIO)
import Prelude

import Config
-----------------------
-------- TYPES --------
-----------------------
data Section = Info KV
  | ProgramStatus KV
  | HostStatus KV
  | ServiceStatus KV
  | ContactStatus KV
  | UnknownSection String KV
  deriving (Ord, Show, Eq) 

type KV = Map.Map String String

----------------------
-------- CODE --------
----------------------
section_open :: Parser String
section_open = do
  sect <- many letter
  spaces
  string "{"
  return sect

section_close :: Parser (Maybe (String,String))
section_close = do
  spaces
  string "}"
  return Nothing

eol :: Parser ()
eol = do 
            oneOf "\n\r"
            return ()
     <?> "end of line"

vchar :: CharParser st Char
vchar = satisfy (\c -> (c >= chr 33) && (c <= chr 126))
                   <?> "printable character"

-- Matches identifier of character|_
ident :: Parser String
ident = do 
          cs <- many (letter <|> digit <|> char '.' 
            <|> char ',' <|> char '_')
          return cs
        <?> "identifier"

-- Matches identifier of character|_
ident_ws :: Parser String
ident_ws = do 
           cs <- many (vchar <|> char ' ')
           return (cs)
         <?> "identifier"

item :: Parser (Maybe (String,String))
item = do 
    skipMany space
    key <- ident
    char '='
    value <- ident_ws 
    return $ Just (key, rstrip value)
  <?> "item"
  where rstrip = reverse . dropWhile isSpace . reverse

comment :: Parser (Maybe (String,String))
comment = do 
    char '#'
    skipMany  (noneOf "\r\n")
    eol
    return Nothing
  <?> "comment"


block::Parser Section
block = do
     many eol
     many comment
     many eol
     s <- section_open <* (many eol) 
     l <- (many (choice [try (section_close <* many eol),try comment,try (item <* eol)]))
     return (case s of
       "info" -> (Info  (listToMap $ (catMaybes l)))
       "programstatus" -> ProgramStatus  (listToMap $ (catMaybes l))
       "hoststatus" -> HostStatus  (listToMap $ (catMaybes l))
       "servicestatus" -> ServiceStatus  (listToMap $ (catMaybes l))
       "contactsatus" -> ContactStatus  (listToMap $ (catMaybes l))
       _ -> UnknownSection s (listToMap $ (catMaybes l)))

listToMap :: [(String, String)] -> KV
listToMap = Prelude.foldr (uncurry Map.insert) Map.empty

readStatus:: String -> IO (Either ParseError [Section])
readStatus fileName = do
  sections <-  (parseFromFile (many (block)) fileName)
  return sections

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

number = many1 digit

plus = char '+' *> number

minus = char '-' <:> number

integer = plus <|> minus <|> number

float = fmap rd $ integer <++> decimal <++> exponent
    where rd       = read :: String -> Float
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> integer
-- over simplistic parsing of perfomance data
-- need to handle other parameters like size
performanceData:: Parser Float 
performanceData = string "time=" *> float <* char 's' <* string ";;;" <* spaces 

parsePreformanceData::String->IO (Either ParseError Float)
parsePreformanceData d = do
  return $ parse performanceData "(data)" d
