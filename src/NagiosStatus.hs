{-# LANGUAGE OverloadedStrings #-}
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
-----------------------
-------- TYPES --------
-----------------------

data LogLine = LogLine {
      getIP     :: String
    , getIdent  :: String
    , getUser   :: String
    , getDate   :: String
    , getReq    :: String
    , getStatus :: String
    , getBytes  :: String
    , getRef    :: String
    , getUA     :: String
} deriving (Ord, Show, Eq)

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

performanceData:: Parser [(String,String)] 
performanceData = do
  ppd <- many item <* string ";;;" <* spaces
  return (catMaybes ppd)

parsePreformanceData::String->IO (Either ParseError [(String,String)])
parsePreformanceData d = do
  return $ parse performanceData "(data)" d
