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
module Main where

import ActionhaXe.Lexer

import System.Environment (getArgs)
import Text.PrettyPrint

format:: Token -> Doc
format t = parens (hcat [text (tokenSource t), space, int (tokenLine t), colon, int (tokenCol t) ]) <+> text (tokenItemS t)

unknowns ts = [ format t  | t@(s, TokenUnknown a) <- ts]
comments ts = [ format t | t@(s, TokenComment a) <- ts]
strings ts = [ format t | t@(s, TokenString a) <- ts]
xmls ts = [ format t | t@(s, TokenXml a) <- ts]

main = do args <- getArgs
          let filename = args!!0
          contents <- readFile filename
          let tokens = runLexer "" contents
          putStrLn $ render $ hcat $ punctuate (text "\n") $ unknowns tokens
          putStrLn "\n\nTokenized--"
          print tokens

