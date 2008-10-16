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
import Text.Parsec.Pos
import System.Environment (getArgs)
import Text.PrettyPrint
import System.Directory
import Data.Char
import Data.List
import Control.Monad

format s a = parens (hcat [text (sourceName s), space, int (sourceLine s), colon, int (sourceColumn s) ]) <+> text a

{-
unknowns ts = [ format s a  | t@(s, TokenUnknown a) <- ts]
comments ts = [ format s a | t@(s, TokenComment a) <- ts]
strings ts = [ format s a | t@(s, TokenString a) <- ts]
xmls ts = [ format s a | t@(s, TokenXml a) <- ts]
regexs ts = [ format s (r1++"/"++r2) | t@(s, TokenRegex (r1,r2)) <- ts]
-}

unknowns ts = [ t  | t@(s, TokenUnknown a) <- ts]
comments ts = [ t | t@(s, TokenComment a) <- ts]
strings ts = [ t | t@(s, TokenString a) <- ts]
xmls ts = [ t | t@(s, TokenXml a) <- ts]
regexs ts = [ t | t@(s, TokenRegex (r1,r2)) <- ts]

inRange (s, _) (t, _) = linet > lines - 3 && linet < lines + 3
    where
           linet = sourceLine t
           lines = sourceLine s

getContext ftoken tokens = filter (\t -> inRange ftoken t) tokens

displayToken tokens t = do putStrLn "Detected Unknown: "
                           print t
                           putStrLn "Context --"
                           mapM_ print $ getContext t tokens

renderTokens lfilter filename = 
    do contents <- readFile filename
       putStrLn $ "Checking: "++filename
       let tokens = runLexer filename contents
       let ftokens = lfilter tokens
       if null ftokens == False
           then do putStrLn $ "Detected: "++filename
                  -- putStrLn $ render $ hcat $ punctuate (text "\n") $ ftokens
                   mapM_ (displayToken tokens) ftokens
           else return ()

isFile f = do t <- doesFileExist f
              return $ t && ("as" == (map toLower $ reverse $ take 2 $ reverse f))

isDir d = do t <- doesDirectoryExist d
             return $ t && d /= "." && d /= ".." && d /= ".svn"

getASFiles dir = do contents <- getDirectoryContents dir
                    let c = map (\e -> dir++"/"++e) (filter (\d-> d /= "." && d /="..") contents)
                    asfiles <- filterM isFile c
                    asdirs <- filterM isDir c
                    childAsFiles <- mapM getASFiles asdirs
                    if null asfiles 
                        then return $ concat childAsFiles
                        else return $ concat (asfiles: childAsFiles)


main = do args <- getArgs
          if isSuffixOf ".as" (args!!0)
              then renderTokens unknowns $ args!!0
              else do asFiles <- getASFiles (args!!0)
                      mapM_ (renderTokens unknowns) asFiles

{-
main = do args <- getArgs
          renderTokens unknowns $ args!!0
-}
