-- Turn the as3 source into Tokens for parsing

import Text.Parsec
--import Control.Monad.Identity

type Token = (SourcePos, String)
type Parser = Parsec Token ()

--lexer :: ParsecT String () Identity [Token]
lexer = many (do { p <- getPosition; char 'c'; return (p, "test") })

runLexer :: String -> [Token]
runLexer s = case parse lexer "" s of
                  Right l -> l
                  Left _ -> []

main = print $ runLexer "c"
