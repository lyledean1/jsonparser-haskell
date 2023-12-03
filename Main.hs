{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO
import Data.Char ( isDigit, isSpace ) 
import Control.Applicative ( Alternative((<|>), empty, many) )
import Distribution.Compat.CharParsing (CharParsing(string))

data JsonValue = 
        JsonNull | 
        JsonBool Bool | 
        JsonNumber Integer | -- no float
        JsonString String | --- no escape 
        JsonArray [JsonValue] | 
        JsonObject [(String, JsonValue)] 
        deriving (Show, Eq)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a)}

instance Functor Parser where 
        fmap f (Parser p) = Parser $ \input -> do 
                (input', x) <- p input
                Just (input', f x) 

instance Applicative Parser where 
        pure x = Parser $ \input -> Just (input, x)
        (Parser p1) <*> (Parser p2) = Parser $ \input -> do 
                (input', f) <- p1 input
                (input'', a) <- p2 input'
                Just (input'', f a)

instance Alternative Parser where 
        empty = Parser $ \_ -> Nothing 
        Parser p1 <|> Parser p2 = 
                Parser $ \input -> p1 input <|> p2 input


stringP :: String -> Parser String 
stringP = sequenceA . map charP

charP :: Char -> Parser Char 
charP x = Parser $ \case
        y:ys | y == x -> Just (ys, x)
        _ -> Nothing

spanP :: (Char -> Bool) -> Parser String
spanP f = 
        Parser $ \input -> 
                let (token, rest) = span f input 
                in Just(rest, token)

ws :: Parser String 
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = 
        Parser $ \input -> do
        (input', xs) <- p input 
        if null xs 
                then Nothing
                else Just (input', xs )

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false") where 
        f "true" = JsonBool True
        f "false" = JsonBool False 
        f _       = undefined

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
        where f ds = JsonNumber $ read ds 

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue 
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
        where elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' 
        *> ws *>  
        sepBy (ws *> charP ',' <* ws) pair 
        <* ws <* 
        charP '}')
        where 
                pair = (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> charP ':' *> ws) <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

parseFile :: FilePath                 -- File path to parse
          -> Parser a                 -- Parser to use
          -> IO (Maybe a)

parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)
  

main :: IO ()
main = do
    result <- parseFile "./example.json" jsonValue
    case result of
        Just value -> print value -- or handle the parsed value as needed
        Nothing -> putStrLn "Parsing failed."