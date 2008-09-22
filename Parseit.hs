-- Parse a file

import ActionhaXe.Lexer
import ActionhaXe.Prim
import ActionhaXe.Parser
import System.Environment (getArgs)



main = do args <- getArgs
          let filename = args!!0
          contents <- readFile filename
          let tokens = runLexer "" contents
          let ast = parseTokens filename tokens
          print ast

