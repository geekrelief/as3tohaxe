{-
    as3tohaxe - An Actionscript 3 to haXe source file translator
    Copyright (C) 2008 Don-Duong Quach

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
-- Turn the as3 source into Tokens for parsing

module ActionhaXe.Lexer (runLexer, Token, tokenSource, tokenLine, tokenCol, tokenItem, tokenItemS, TPos(..), TokenType(..), TokenNum(..), keywords, operators) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Perm
import Data.Char
import Data.List 
import Data.Generics -- not Haskell '98

type Token = (TPos, TokenType)

data TPos = TPos SourceName Line Column
    deriving (Show, Eq, Ord, Data, Typeable)

tokenSource ((TPos s l c), i) = s
tokenLine ((TPos s l c), i) = l
tokenCol ((TPos s l c), i) = c
tokenItem (s, i) = i

tokenItemS (p, i) = case i of
                        TokenWhite   s -> s
                        TokenComment s -> s 
                        TokenNum s -> case s of
                                          TokenInteger n -> n
                                          TokenDouble  n -> n
                                          TokenOctal   n -> n
                                          TokenHex     n -> n
                        TokenIdent   s -> s
                        TokenString  s -> s
                        TokenNl      s -> s
                        TokenEscaped s -> s
                        TokenXml     s -> s
                        TokenKw      s -> s
                        TokenOp      s -> s
                        TokenUnknown s -> s

data TokenNum    = TokenInteger String
                 | TokenDouble String
                 | TokenOctal String
                 | TokenHex String
    deriving (Show, Eq, Ord, Data, Typeable)

data TokenType = 
             TokenWhite String
           | TokenComment String
           | TokenNum TokenNum
           | TokenIdent String
           | TokenString String
           | TokenNl String
           | TokenEscaped String
           | TokenKw String
           | TokenOp String
           | TokenXml String
           | TokenUnknown String
    deriving (Show, Eq, Ord, Data, Typeable)

keywords = [ "...", "as", "break", "case", "catch", "class", "const", "continue", "default",
             "delete", "do", "else", "extends", "false", "finally", "for", "function", "if",
             "implements", "import", "in", "instanceof", "interface", "internal", "is", 
             "native", "new", "null", "package", 
             "private", "protected", "public", "return", "super", "switch", "this", "throw", 
             "true", "try", "typeof", "use", "undefined", "var", "void", "while", "with",
-- syntactic keywords
             "each", "get", "set", "namespace", "include", "dynamic", "final", "native",
             "override", "static"
           ]

operators = [ "...", ".", "[", "]", "(", ")", "@", "::", "..", "{", "}", 
              "++", "--", "-", "~", "!", "*", "/", "%",
              "+", "-", "<<", ">>", ">>>", "<", "<=", ">", ">=",
              "==", "!=", "===", "!==", "&", "^", "|", "&&", "||",
              "?", "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=",
              ">>>=", "&=", "^=", "|=", ",", ":", ";"
            ]

keyword = do { x <- many1 identChar; if (elem x $ keywords) then return (TokenKw x) else unexpected "keyword" }

identifier = do{ x<- (satisfy (\c -> isAlpha c || c == '_' || c == '$')); x' <- many identChar; return $ TokenIdent $ [x]++x' }

identChar = satisfy (\c -> isAlphaNum c || c == '_' || c == '$')

sortByLength = sortBy (\x y -> compare (length y) (length x))

operator' (o:os) = try (do{ s <- string o; return $ TokenOp s })
               <|> operator' os
operator' []     = fail " failed "

operator = operator' $ sortByLength operators

utf8 = do { c <- string "\239\187\191"; return $ TokenWhite c }

simpleSpace = many1 (satisfy (\c -> c == ' ' || c == '\t'))
whiteSpace = do{ x <- many1 ( try( simpleSpace ) <|>  nl' ); return $ TokenWhite $ concat x}

anyCharButNl = do{ c <- (satisfy(\c-> isPrint c && c /= '\r' && c /= '\n')); return c }

escapedAnyChar = try(do{ char '\\'; c <- anyCharButNl; return $ "\\"++[c]})
             <|> do{ c <- anyCharButNl; return [c]}

escapedCharToken = do{ char '\\'; c <- anyCharButNl; return $ TokenEscaped $ "\\"++[c]}

nl' = do{ try (string "\r\n"); return "\r\n" }
 <|> do{ try (char '\n'); return "\n" }
 <|> do{ try (char '\r'); return "\r" }

nl = do{ x <- nl'; return $ TokenNl x}

quotedDString = do{ char '"'; s <- manyTill escapedAnyChar (char '"'); return $ TokenString $ "\"" ++ (concat s) ++ "\""}
quotedSString = do{ char '\''; s <- manyTill escapedAnyChar (char '\''); return $ TokenString $ "'" ++ (concat s) ++ "'"}

commentSLine = do{ string "//"; s <- manyTill anyChar (lookAhead nl); return $ TokenComment $ "//"++s}
commentMLine = do{ string "/*"; s <- manyTill anyChar (try(string "*/")); return $ TokenComment $ "/*"++s++"*/" }

xml = do{ char '<'; t <- many1 (satisfy (\c -> isPrint c && c /= '>')); char '>'; x <- manyTill anyChar (try (string $ "</"++t++">")); return $ TokenXml $ "<"++t++">"++x++"</"++t++">"}

xmlSTag = do{ char '<'; t <- manyTill (satisfy (\c -> isPrint c && c /= '/' && c /= '>')) (string "/>"); return $ TokenXml $ "<"++t++"/>"}

number = try (do{ char '0'; char 'x'; x <- many1 hexDigit; return $ TokenNum $ TokenHex $ "0x"++x})
     <|> try (do{ char '0'; x <- many1 octDigit; return $ TokenNum $ TokenOctal $ "0"++x})
     <|> try (do{ x <- double; return $ TokenNum $ TokenDouble $  x })
     <|> do{ x <- many1 digit; return $ TokenNum $ TokenInteger $ x }

double = try ( do{ h <- decimalInt; char '.'; t <- optionMaybe decimalInt; e <- optionMaybe expPart; return $ h++"."++(maybe "" id t)++(maybe "" id e) })
     <|> try ( do{ char '.'; t <- decimalInt; e <- optionMaybe expPart; return $ "."++t++(maybe "" id e)})
     <|> do{ h <- decimalInt; e <- expPart; return $ h++e}

decimalInt = do{ char '0'; return "0"}
         <|> do{ h <- nonZeroDigit; t <- many digit; return $ h++t}

nonZeroDigit = do{ x<- oneOf "123456789"; return [x]}

expPart = do{ e <- oneOf "eE"; i <- signedInt; return $ [e]++i}

signedInt = do{ s <- optionMaybe (oneOf "+-"); i <- many1 digit; return $ (maybe "" (\x -> [x]) s) ++ i}

atoken = 
         try (do{ x <- utf8; return x})
     <|> try (do{ x <- keyword; return x})
     <|> try (do{ x <- commentSLine; return x})
     <|> try (do{ x <- commentMLine; return x})
     <|> try (do{ x <- xmlSTag; return x})
     <|> try (do{ x <- xml; return x})
     <|> try (do{ x <- escapedCharToken; return x})
     <|> try (do{ x <- number; return x})
     <|> try (do{ x <- operator; return x})
     <|> try (do{ x <- identifier; return x})
     <|> try (do{ x <- quotedDString; return x})
     <|> try (do{ x <- quotedSString; return x})
     <|> try (do{ x <- whiteSpace; return x}) 
     <|>      do{ x <- anyToken; return $ TokenUnknown $ x:[]}


lexer = many1 (do { p <- getPosition; t <- atoken; return (TPos (sourceName p) (sourceLine p) (sourceColumn p), t) })

runLexer :: String -> String -> [Token]
runLexer filename s = case parse lexer filename s of
                  Right l -> l
                  Left _ -> []
