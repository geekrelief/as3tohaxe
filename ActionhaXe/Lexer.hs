-- Turn the as3 source into Tokens for parsing

module ActionhaXe.Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Data.Char
import Data.List

--import Control.Monad.Identity


data TokenNum    = TokenInteger Integer
                 | TokenDouble Double
    deriving (Show, Eq)


data Token = 
             TokenWhite String
           | TokenComment String
           | TokenNum TokenNum
           | TokenIdent String
           | TokenStringLit String
           | TokenNl
           | TokenRegex (String, String)
           | TokenEof
           | TokenKw String
           | TokenOp String
           | TokenXML String
           | TokenUnknown String
           | TokenRest String
    deriving (Show, Eq)

keywords = [ "as", "break", "case", "catch", "class", "const", "continue", "default",
             "delete", "do", "else", "extends", "false", "finally", "for", "function", "if",
             "implements", "internal", "is", "native", "new", "null", "package", "private",
             "protected", "public", "return", "super", "switch", "this", "throw", "to",
             "true", "try", "typeof", "use", "var", "void", "while", "with",
-- syntactic keywords
             "each", "get", "set", "namespace", "include", "dynamic", "final", "native",
             "override", "static"
           ]

operators = [ ".", "[", "]", "(", ")", "@", "::", "..", "{", "}", 
              "++", "--", "-", "~", "!", "*", "/", "%",
              "+", "-", "<<", ">>", ">>>", "<", "<=", ">", ">=",
              "==", "!=", "===", "!==", "&", "^", "|", "&&", "||",
              "?", "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=",
              ">>>=", "&=", "^=", "|=", ",", ":", ";"
            ]

keyword :: Parser String
keyword = do { x <- many1 identChar; if (elem x $ keywords) then return x else unexpected "keyword" }

identifier :: Parser String
identifier = do{ x <- many1 identChar; return x }

identChar = satisfy (\c -> isAlphaNum c || c == '_' || c == '$')

sortByLength = sortBy (\x y -> compare (length y) (length x))

operator' (o:os) = try (do{ s <- string o; return s })
               <|> operator' os
operator' []     = fail " failed "

operator = operator' $ sortByLength operators

quotedDString = do{ s <- between (char '"') (char '"') (many (satisfy (\c -> isPrint c && c /= '"'))); return $ "\"" ++ s ++ "\""}
quotedSString = do{ s <- between (char '\'') (char '\'') (many (satisfy (\c -> isPrint c && c /= '\''))); return $ "'" ++ s ++ "'"}

commentSLine = do{ s <- between (string "//") newline (many (satisfy(\c -> c /= '\n'))); i <- getInput; setInput $ "\n"++i ; return $ "//"++s}
commentMLine = do{ string "/*"; s <- manyTill anyChar (try(string "*/")); return $ "/*"++s++"*/" }

--type Parser = Parsec Token ()
whiteSpace :: Parser String
whiteSpace = many1 (satisfy (\c -> isSpace c && c /= '\n'))


--atoken :: String -> Token
atoken = 
         try (do{ x <- many1 digit; char '.'; y <- many1 digit; return $ TokenNum $ TokenDouble $ read (x++"."++y)  })
     <|> try (do{ x <- many1 digit; return $ TokenNum $ TokenInteger $ read x })
     <|> try (do{ x <- keyword; return $ TokenKw x})
     <|> try (do{ x <- commentSLine; return $ TokenComment x})
     <|> try (do{ x <- commentMLine; return $ TokenComment x})
     <|> try (do{ x <- operator; return $ TokenOp x})
     <|> try (do{ x <- identifier; return $ TokenIdent x})
     <|> try (do{ x <- quotedDString; return $ TokenStringLit x})
     <|> try (do{ x <- quotedSString; return $ TokenStringLit x})
     <|> try (do{ x <- whiteSpace; return $ TokenWhite x}) 
     <|> try (do{ char '\n'; eof; return TokenEof })
     <|> try (do{ char '\n'; return TokenNl })
     <|> try (do{ x <- anyToken; return $ TokenUnknown $ x:[]})


--lexer :: ParsecT String () Identity [Token]
lexer = many1 (do { p <- getPosition; t <- atoken; return (p, t) })

runLexer :: String -> [(SourcePos, Token)]
runLexer s = case parse lexer "" s of
                  Right l -> l
                  Left _ -> []

--unknowns t = [ x | x@(_, TokenUnknown a) <- t]

--main = print $ runLexer "   12  3.1   3  \n 123 \t\t"
{-
main = do args <- getArgs
          contents <- readFile args!!1
          let tokens = runLexer contents
          putStrLn "Unknown Tokens--\n"
          print $ unknowns tokens
          putStrLn "\n\nTokenized--"
          print tokens
-}
--main = putStrLn "test"
