module Main where

import Data.List.Split
import System.IO 
import Data.Char(toUpper)
import Text.ParserCombinators.Parsec

csvParser :: Parser [[String]]
csvParser = do
   rows <- sepBy row (char '\n')
   return rows

row :: Parser [String]
row = do
   cols <- sepBy cell (char ',')
   return cols

cell :: Parser String
cell = do
   char '"'
   content <- many (noneOf "\"")
   char '"'
   return content

main :: IO ()
main = do
   let input = "col1,col2,col3\nr2 c1,r2 c2,r2 c3\n\"r3,c1\",\"r3,c2\",\"r3,c3\""
   let result = parse csvParser "" input
   case result of
     Right parsed -> print parsed
     Left err -> print err

line :: Parser [String]
line = do
  fields <- field sepBy char ","
  return fields

field :: Parser String
field = quotedField <|> nonQuotedField

quotedField :: Parser String
quotedField = do
   char '"'
   content <- many (noneOf '"')
   char '"'
   return content

nonQuotedField :: Parser String
nonQuotedField = many (noneOf ",\n")

eol :: Parser String
eol = try (string "\n\r") <|> try (string "\r\n") <|> string "\n" <|> string "\r"