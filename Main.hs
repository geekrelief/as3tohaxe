module Main where

import ActionhaXe.Lexer
import System.Environment (getArgs)

unknowns ts = [ t | t@(_, TokenUnknown a) <- ts]
comments ts = [ t | t@(_, TokenComment a) <- ts]
strings ts = [ t | t@(_, TokenStringLit a) <- ts]

main = do args <- getArgs
          contents <- readFile (args!!0)
          let tokens = runLexer contents
          print $ comments tokens
          putStrLn "\n\nTokenized--"
          print tokens
