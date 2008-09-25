-- Translate a file

import ActionhaXe.Lexer
import ActionhaXe.Prim
import ActionhaXe.Parser
import ActionhaXe.Translator
import System.Environment (getArgs)


main = do args <- getArgs
          let filename = args!!0
          contents <- readFile filename
          let tokens = runLexer "" contents
          ast <- case parseTokens filename tokens of
                        Right ast -> return ast
                        Left err  -> fail $ show err
          let trans = translateAs3Ast ast
          -- write out (maybe hxml) and hx, both should be Strings
          print $ snd trans
