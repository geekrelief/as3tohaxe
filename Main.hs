module Main where

import ActionhaXe.Lexer
import Text.Parsec.Pos
import System.Environment (getArgs)
import Text.PrettyPrint

unknowns ts = [ (parens (hcat [int (sourceLine s), colon, int (sourceColumn s) ]) <+> text a ) | t@(s, TokenUnknown a) <- ts]
comments ts = [ t | t@(_, TokenComment a) <- ts]
strings ts = [ t | t@(_, TokenStringLit a) <- ts]

main = do args <- getArgs
          contents <- readFile (args!!0)
          let tokens = runLexer contents
          putStrLn $ render $ hcat $ punctuate (text "\n") $ unknowns tokens
--          putStrLn "\n\nTokenized--"
--          print tokens
--          print pprint


pprint = render (text "Test" <+> text "Run")
