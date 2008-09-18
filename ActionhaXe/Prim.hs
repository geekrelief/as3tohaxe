module ActionhaXe.Prim where

import ActionhaXe.Lexer
import Text.Parsec
import Text.Parsec.Prim

type AsParser = Parsec [Token] ()

mytoken :: (Token -> Maybe a) -> AsParser a
mytoken test = token showTok posFromTok testTok
             where
                 showTok (pos, t)    = show t
                 posFromTok (pos, t) = pos
                 testTok t           = test t

anytok = mytoken (\tok -> Just tok)

kw k = mytoken ( \t@(s, tok) -> case tok of
                                    TokenKw k' | k == k' -> Just t
                                    _ -> Nothing)


--op = mytoken $ \tok -> case tok of
                                
--id = mytoken $ \tok ->
