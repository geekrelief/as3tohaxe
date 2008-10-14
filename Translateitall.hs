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
    contents <- readFile filename
    let tokens = runLexer "" contents
    program <- case parseTokens filename tokens of
        Right (Program ast st) -> return (ast, st)
        Left err  -> fail $ show err
    trans <- runStateT (translateAs3Ast (fst program)) (snd program)
    let outfilename = outdir ++ (reverse $ "xh" ++ ( drop 2 $ reverse filename))
    writeFile outfilename $ fst trans

isFile f = do t <- doesFileExist f
              return $ t && ("as" == (map toLower $ reverse $ take 2 $ reverse f))

isDir d = do t <- doesDirectoryExist d
             return $ t && d /= "." && d /= ".."

ifM f b = if not b then f else return ()

translateDir dir = do 
    contents <- getDirectoryContents dir
    dirExists <- doesDirectoryExist (outdir ++ dir) 
    ifM (createDirectoryIfMissing True (outdir++dir)) dirExists
    let c = map (\e -> dir++"/"++e) (filter (\d-> d /= "." && d /="..") contents)
    asfiles <- filterM isFile c
    asdirs <- filterM isDir c
    mapM_ translateFile asfiles
    mapM_ translateDir asdirs

main = do args <- getArgs
          if isSuffixOf ".as" (args!!0)
              then translateFile $ args!!0
              else translateDir $ args!!0
