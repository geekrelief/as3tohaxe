module Main where

import ActionhaXe.Lexer
import Text.Parsec.Pos
import System.Environment (getArgs)
import Text.PrettyPrint

format s a = parens (hcat [text (sourceName s), space, int (sourceLine s), colon, int (sourceColumn s) ]) <+> text a

unknowns ts = [ format s a  | t@(s, TokenUnknown a) <- ts]
comments ts = [ format s a | t@(s, TokenComment a) <- ts]
strings ts = [ format s a | t@(s, TokenString a) <- ts]
xmls ts = [ format s a | t@(s, TokenXml a) <- ts]

main = do args <- getArgs
          let filename = args!!0
          contents <- readFile filename
          let tokens = runLexer filename contents
          putStrLn $ render $ hcat $ punctuate (text "\n") $ unknowns tokens
--          putStrLn "\n\nTokenized--"
--          print tokens

