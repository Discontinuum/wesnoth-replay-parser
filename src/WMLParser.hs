{-# LANGUAGE FlexibleContexts#-}
module WMLParser where

import Text.ParserCombinators.Parsec
import Debug.Trace
import qualified Data.Map as M
import Data.List (intercalate)

type WML_Doc = [WML_Entity]
data WML_Entity = Tag String WML_Doc | Attr [String] [String] deriving Show

getTagName (Tag name _) = name
getTagValue (Tag _ val) = val
getAttrValue (Attr _ val) = val

textdomain :: Parser String
textdomain = string "#textdomain" >> spaces >> (many1 $ oneOf "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM0987654321_-")

stringW = do
  char '"'
  ans <- many escapedChar
  char '"'
  return ans
  where escapedChar = (try $ string "\"\"" >> return '"') <|> noneOf "\""

raw_string = do
  string "<<"
  ans <- many escapedRawChar
  string ">>"
  return ans
  where escapedRawChar = (try $ char '>' >> lookAhead (noneOf ">") >> return '>') <|> noneOf ">"

textW = many $ noneOf "+\n\""

wml_value_component = (try $ (optional $ char '_' >> spaces) >> (stringW <|> raw_string)) <|> textW

wml_value = sepBy1 wml_value_component (try $ spaces >> char '+' >> spaces >> (optional textdomain) >> spaces)

wml_name = many1 $ oneOf "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM0987654321_"

wml_key_seq = sepBy1 wml_name (try $ spaces >> char ',' >> spaces)

wml_attribute = do
  optional (textdomain >> spaces)
  keys <- wml_key_seq
  spaces
  char '='
  spaces
  val <- wml_value
  return $ Attr keys val

wml_tag = do
  lookAhead (try (char '[' >> noneOf "/"))
  char '['
  tag_name <- wml_name
  char ']'
  content <- wml_doc
  string "[/"
  string tag_name
  char ']'
  return $ Tag tag_name content

wml_doc = do
  spaces
  ans <- endBy (wml_tag <|> wml_attribute) (try spaces)
  spaces
  return ans

parseWML = parse wml_doc ""
parseX x a = parse x "" a

getTagsByName :: String -> WML_Doc -> [WML_Entity]
getTagsByName name doc = filter f doc
  where f (Tag name' _) = name' == name
        f _ = False
        
getTagByName name doc = 
  case getTagsByName name doc of 
    [] -> Nothing
    h:_ -> Just h
        
getAttrsMap :: WML_Doc -> M.Map String String
getAttrsMap doc = M.fromList $ map f (filter p doc)
  where p (Attr _ _) = True
        p _ = False
        f (Attr name val) = (intercalate "," name, intercalate "," val)

isTrue "yes" = True
isTrue "true" = True
isTrue _ = False
