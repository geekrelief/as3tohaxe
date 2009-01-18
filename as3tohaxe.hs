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
import ActionhaXe.CLArgs

import System.Directory
import System.Environment (getArgs)
import Control.Monad.State
import System.Exit
import Data.Char (toUpper, toLower)
import Data.List (isSuffixOf, intercalate)
import System.Console.ParseArgs
import Data.Maybe (fromJust, fromMaybe)
import System.FilePath.Posix (takeDirectory, takeBaseName, splitDirectories)
import qualified Data.Map as Map

translateFile :: String -> StateT Conf IO ()
translateFile filename = do
    conf <- get
    let outdir = confOutput conf
    let dir = takeDirectory filename
    dirExists <- liftIO $ doesDirectoryExist (outdir ++ dir) -- check if dir exists
    -- create dir
    liftIO $ unless dirExists (createDirectoryIfMissing True (outdir++dir) >> putStrLn ("Created " ++ outdir++dir))
    -- add path to imports
    unless dirExists (put conf{ imports = (Map.insert dir [] $ imports conf) })
    -- add class to imports
    put conf{imports = (Map.insertWith (\x y -> x ++ y) dir [takeBaseName $ (toUpper $ head filename):(tail filename)] $ imports conf) }
    liftIO $ putStrLn $ "Translating " ++ filename
    contents <- liftIO $ readFile filename
    let updated_contents = if gotArg (confArgs conf) NoCarriage
                               then filter ( /= '\r' ) contents -- remove carriage
                               else contents
    let tokens = runLexer "" updated_contents
    let outfilename = outdir ++ (reverse $ "xh" ++ ( drop 2 $ reverse filename))
    program <- case parseTokens filename tokens of
        Right p@(AS3Program x st) -> return (p, st{outfile = outfilename, conf=conf })
        Right p@(AS3Directives x st) -> return (p, st{outfile = outfilename, conf=conf })
        Left err  -> fail $ show err
    trans <- liftIO $ runStateT (translateAs3Ast (fst program)) (snd program)
    liftIO $ writeFile outfilename $ fst trans

isFile :: String -> StateT Conf IO Bool
isFile f = do conf <- get
              let outdir = confOutput conf
              t <- liftIO $ doesFileExist f
              let outfilename = outdir ++ (reverse $ "xh" ++ ( drop 2 $ reverse f))
              o <- liftIO $ doesFileExist (outfilename)
              when o $ liftIO $ putStrLn $ "Skipping " ++ f
              return $ not o && t && ("as" == (map toLower $ reverse $ take 2 $ reverse f))

isDir :: String -> StateT Conf IO Bool
isDir d = do t <- liftIO $ doesDirectoryExist d
             return $ t

translateDir :: String -> StateT Conf IO ()
translateDir dir = do 
    conf <- get
    let outdir = confOutput conf
    contents <- liftIO $ getDirectoryContents dir
    let c = map (\e -> dir++"/"++e) (filter (\d-> d /= "." && d /=".." && d /= ".svn") contents)
    asfiles <- filterM isFile c
    asdirs <- filterM isDir c
    mapM_ translateFile asfiles
    mapM_ translateDir asdirs

main = do args <- parseArgsIO ArgsTrailing clargs
          let input = fromJust $ getArgString args Input 
          let outdir = fromMaybe "hx_output/" $ getArgString args OutputDir 
          let conf = Conf{ confArgs = args , confInput = input, confOutput = outdir, imports = Map.empty}
          (_, updated_conf) <- if isSuffixOf ".as" input
                              then do dirExists <- doesDirectoryExist outdir
                                      unless dirExists ((createDirectoryIfMissing True outdir) >> putStrLn ("Created " ++ outdir))
                                      runStateT (translateFile input) conf
                              else runStateT (translateDir input) conf
          if gotArg (confArgs updated_conf) CreateImports
              then do putStrLn $ "Creating import files in "++outdir
                      mapM_ (\(k, v)-> createImport outdir k $ reverse v) $ Map.toList $ imports updated_conf
              else return () 

createImport outdir packagePath packageList = do let importFileName = intercalate "_" ("Import":(splitDirectories packagePath))
                                                 writeFile (outdir ++ importFileName++".hx") $ packageContent packagePath packageList
    where packageContent path list = foldl (\content klass -> content++ "typedef "++klass++" = "++ppath++klass++"\n") "" list
                                         where ppath = (intercalate "." $ splitDirectories path)++"."
                                         
