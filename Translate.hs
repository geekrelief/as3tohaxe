-- Translate a file

import ActionhaXe.Lexer
import ActionhaXe.Prim
import ActionhaXe.Parser
import ActionhaXe.Translator
import System.Environment (getArgs)

import Control.Monad.State


main = do args <- getArgs
          let filename = args!!0
          contents <- readFile filename
          let tokens = runLexer "" contents
          program <- case parseTokens filename tokens of
                        Right (Program ast st) -> return (ast, st)
                        Left err  -> fail $ show err
          let trans = runState (translateAs3Ast (fst program)) (snd program)
          print $ fst trans
