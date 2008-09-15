-- Turn the as3 source into Tokens for parsing

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Data.Char

--import Control.Monad.Identity

data Token = 
             TokenWhite String
           | TokenComment String
           | TokenInt Int
           | TokenIdent String
           | TokenStringLit String
           | TokenNL
           | TokenRegex (String, String)
           | TokenEof
           | TokenRID String
           | TokenROP String
           | TokenXML String
  deriving (Show, Eq)

--type Parser = Parsec Token ()
whiteSpace :: Parser String
whiteSpace = many1 (satisfy (\c -> isSpace c && c /= '\n'))


--atoken :: String -> Token
atoken = try (do{ x <- many1 digit; return $ TokenInt $ read x })
    <|>  try (do{ x <- whiteSpace; return $ TokenWhite x}) 
    <|>  try (do{ char '\n'; return TokenNL })

         --try (do{ eof; return TokenEof})
--         <?> "unidentifiable token"


--lexer :: ParsecT String () Identity [Token]
lexer = many (do { p <- getPosition; t <- atoken; return (p, t) })

runLexer :: String -> [(SourcePos, Token)]
runLexer s = case parse lexer "" s of
                  Right l -> l
                  Left _ -> []

main = print $ runLexer "   12    3  \n 123 \t\t"

--main = putStrLn "test"
