module Main where

import ActionhaXe.Lexer
import Text.Parsec.Pos
import System.Environment (getArgs)
import Text.PrettyPrint
import System.Directory
import Data.Char
import Control.Monad

format s a = parens (hcat [text (sourceName s), space, int (sourceLine s), colon, int (sourceColumn s) ]) <+> text a

unknowns ts = [ format s a  | t@(s, TokenUnknown a) <- ts]
comments ts = [ format s a | t@(s, TokenComment a) <- ts]
strings ts = [ format s a | t@(s, TokenString a) <- ts]
xmls ts = [ format s a | t@(s, TokenXml a) <- ts]
regexs ts = [ format s (r1++"/"++r2) | t@(s, TokenRegex (r1,r2)) <- ts]

renderTokens lfilter filename = 
    do contents <- readFile filename
       let tokens = runLexer filename contents
       if null tokens == False
           then do putStrLn $ "Detected: "++filename
                   putStrLn $ render $ hcat $ punctuate (text "\n") $ lfilter tokens
           else return ()

isFile f = do t <- doesFileExist f
              return $ t && ("as" == (map toLower $ reverse $ take 2 $ reverse f))

isDir d = do t <- doesDirectoryExist d
             return $ t && d /= "." && d /= ".."

getASFiles dir = do contents <- getDirectoryContents dir
                    let c = map (\e -> dir++"/"++e) (filter (\d-> d /= "." && d /="..") contents)
                    asfiles <- filterM isFile c
                    asdirs <- filterM isDir c
                    childAsFiles <- mapM getASFiles asdirs
                    if null asfiles 
                        then return $ concat childAsFiles
                        else return $ concat (asfiles: childAsFiles)

{-
main = do args <- getArgs
          asFiles <- getASFiles (args!!0)
          mapM_ (renderTokens unknowns) asFiles
-}

main = do args <- getArgs
          renderTokens regexs $ args!!0
