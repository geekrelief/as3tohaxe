-- Turn the as3 source into Tokens for parsing

module ActionhaXe.Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Perm
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
           | TokenString String
           | TokenNl
           | TokenRegex (String, String)
           | TokenEof
           | TokenKw String
           | TokenOp String
           | TokenXml String
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

xml = do{ char '<'; t <- many1 (satisfy (\c -> isPrint c && c /= '>')); char '>'; x <- manyTill anyChar (try (string $ "</"++t++">")); return $ "<"++t++">"++x++"</"++t++">"}

xmlSTag = do{ char '<'; t <- manyTill (satisfy (\c -> isPrint c && c /= '/' && c /= '>')) (string "/>"); return $ "<"++t++"/>"}

--type Parser = Parsec Token ()
whiteSpace :: Parser String
whiteSpace = many1 (satisfy (\c -> isSpace c && c /= '\n'))

--regexOptions :: Parser String
regexOptions = permute (catter <$?> ('_', char 'g') <|?> ('_', char 'i') <|?> ('_', char 'm') <|?> ('_', char 's') <|?> ('_', char 'x'))
    where catter g i m s x =  filter (\c -> c /= '_') [g, i, m, s, x]
regex = do { char '/'; x <- noneOf "/*"; s <- manyTill anyChar (try (do {noneOf "\\"; char '/'})); o <- optionMaybe regexOptions; return $ maybe (("/"++(x:[])++s, "")) (\m -> ("/"++(x:[])++s, m)) o}

--atoken :: String -> Token
atoken = 
         try (do{ x <- many1 digit; char '.'; y <- many1 digit; return $ TokenNum $ TokenDouble $ read (x++"."++y)  })
     <|> try (do{ x <- many1 digit; return $ TokenNum $ TokenInteger $ read x })
     <|> try (do{ x <- keyword; return $ TokenKw x})
     <|> try (do{ x <- commentSLine; return $ TokenComment x})
     <|> try (do{ x <- commentMLine; return $ TokenComment x})
     <|> try (do{ x <- xmlSTag; return $ TokenXml x})
     <|> try (do{ x <- xml; return $ TokenXml x})
     <|> try (do{ x <- regex; return $ TokenRegex x})
     <|> try (do{ x <- operator; return $ TokenOp x})
     <|> try (do{ x <- identifier; return $ TokenIdent x})
     <|> try (do{ x <- quotedDString; return $ TokenString x})
     <|> try (do{ x <- quotedSString; return $ TokenString x})
     <|> try (do{ x <- whiteSpace; return $ TokenWhite x}) 
     <|> try (do{ char '\n'; eof; return TokenEof })
     <|> try (do{ char '\n'; return TokenNl })
     <|> try (do{ x <- anyToken; return $ TokenUnknown $ x:[]})


--lexer :: ParsecT String () Identity [Token]
lexer = many1 (do { p <- getPosition; t <- atoken; return (p, t) })

runLexer :: String -> String -> [(SourcePos, Token)]
runLexer filename s = case parse lexer filename s of
                  Right l -> l
                  Left _ -> []
