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
-- Translate a file

import ActionhaXe.Lexer
import ActionhaXe.Data
import ActionhaXe.Prim
import ActionhaXe.Parser
import ActionhaXe.Translator
import System.Directory
import System.Environment (getArgs)
import Control.Monad.State
import System.Exit
import Data.Char (toUpper, toLower)
import Data.List (isSuffixOf)

outdir = "hx_output/"

translateFile filename = do
    putStrLn $ "Translating " ++ filename
    contents <- readFile filename
    let tokens = runLexer "" contents
    let outfilename = outdir ++ (reverse $ "xh" ++ ( drop 2 $ reverse filename))
    program <- case parseTokens filename tokens of
        Right p@(AS3Program x st) -> return (p, st{outfile = outfilename })
        Right p@(AS3Directives x st) -> return (p, st{outfile = outfilename })
        Left err  -> fail $ show err
    trans <- runStateT (translateAs3Ast (fst program)) (snd program)
    writeFile outfilename $ fst trans

isFile f = do t <- doesFileExist f
              let outfilename = outdir ++ (reverse $ "xh" ++ ( drop 2 $ reverse f))
              o <- doesFileExist (outfilename)
              when o $ putStrLn $ "Skipping " ++ f
              return $ not o && t && ("as" == (map toLower $ reverse $ take 2 $ reverse f))

isDir d = do t <- doesDirectoryExist d
             return $ t

translateDir dir = do 
    contents <- getDirectoryContents dir
    dirExists <- doesDirectoryExist (outdir ++ dir) 
    unless dirExists (createDirectoryIfMissing True (outdir++dir) >> putStrLn ("Created " ++ outdir++dir))
    let c = map (\e -> dir++"/"++e) (filter (\d-> d /= "." && d /=".." && d /= ".svn") contents)
    asfiles <- filterM isFile c
    asdirs <- filterM isDir c
    mapM_ translateFile asfiles
    mapM_ translateDir asdirs

main = do args <- getArgs
          if length args == 0
              then do putStrLn "as3tohaxe Copyright (c) 2008 Don-Duong Quach\nUsage: as3tohaxe [directory | filename]"
                      exitWith ExitSuccess
              else return ()
          if isSuffixOf ".as" (args!!0)
              then do
                       dirExists <- doesDirectoryExist outdir
                       unless dirExists ((createDirectoryIfMissing True outdir) >> putStrLn ("Created " ++ outdir))
                       translateFile $ args!!0
              else translateDir $ args!!0
