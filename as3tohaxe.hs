-- Translate a file

import ActionhaXe.Lexer
import ActionhaXe.Data
import ActionhaXe.Prim
import ActionhaXe.Parser
import ActionhaXe.Translator
import System.Directory
import System.Environment (getArgs)
import Control.Monad.State

import Data.Char
import Data.List
import Control.Monad

outdir = "hx_output/"

translateFile filename = do
    putStrLn $ "Translating " ++ filename
    contents <- readFile filename
    let tokens = runLexer "" contents
    program <- case parseTokens filename tokens of
        Right (Program ast st) -> return (ast, st)
        Left err  -> fail $ show err
    trans <- runStateT (translateAs3Ast (fst program)) (snd program)
    let outfilename = outdir ++ (reverse $ "xh" ++ ( drop 2 $ reverse filename))
    writeFile outfilename $ fst trans

isFile f = do t <- doesFileExist f
              let outfilename = outdir ++ (reverse $ "xh" ++ ( drop 2 $ reverse f))
              o <- doesFileExist (outfilename)
              when o $ putStrLn $ "Skipping " ++ f
              return $ not o && t && ("as" == (map toLower $ reverse $ take 2 $ reverse f))

isDir d = do t <- doesDirectoryExist d
             return $ t -- && d /= "." && d /= ".." && d /= ".svn"

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
          if isSuffixOf ".as" (args!!0)
              then do
                       dirExists <- doesDirectoryExist outdir
                       unless dirExists ((createDirectoryIfMissing True outdir) >> putStrLn ("Created " ++ outdir))
                       translateFile $ args!!0
              else translateDir $ args!!0
