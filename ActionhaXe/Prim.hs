module ActionhaXe.Prim where

import ActionhaXe.Lexer
import ActionhaXe.Data
import Text.Parsec
import Text.Parsec.Prim

mytoken :: (Token -> Maybe a) -> AsParser a
mytoken test = token showTok posFromTok testTok
             where
                 showTok (pos, t)    = show t
                 posFromTok (pos, t) = pos
                 testTok t           = test t

anytok' = mytoken $ \t -> Just [t]

white = mytoken $ \t -> case tokenItem t of
                            TokenWhite x -> Just t
                            _ -> Nothing

nl = mytoken $ \t -> case tokenItem t of
                         TokenNl x -> Just t
                         _ -> Nothing

com = mytoken $ \t -> case tokenItem t of
                              TokenComment x -> Just t
                              _ -> Nothing

whiteSpace = many (white <|> nl <|> com)

startWs = do{ w <- whiteSpace; return ([], w)}

mylexeme p = do{ x <- p; w <- whiteSpace; return (x, w)}

num' = mytoken $ \t -> case tokenItem t of
                          TokenNum x -> Just [t]
                          _ -> Nothing

id' = mytoken $ \t -> case tokenItem t of
                         TokenIdent x -> Just [t]
                         _ -> Nothing

mid' i = mytoken $ \t -> case tokenItem t of
                         TokenIdent i' | i == i'  -> Just [t]
                         _ -> Nothing

str' = mytoken $ \t -> case tokenItem t of
                         TokenString x -> Just [t]
                         _ -> Nothing

reg' = mytoken $ \t -> case tokenItem t of
                          TokenRegex x -> Just [t]
                          _ -> Nothing

kw' k = mytoken $ \t -> case tokenItem t of
                           TokenKw k'| k == k' -> Just [t]
                           _ -> Nothing

op' o = mytoken $ \t -> case tokenItem t of
                           TokenOp o' | o == o' -> Just [t]
                           _ -> Nothing

xml' = mytoken $ \t -> case tokenItem t of
                           TokenXml x -> Just [t]
                           _ -> Nothing

num = mylexeme $ num'
str = mylexeme $ str'
reg = mylexeme $ reg'

mid i = mylexeme $ mid' i
kw k = mylexeme $ kw' k
op o = mylexeme $ op' o

-- sepByI includes the separators in the list
sepByI1 :: AsParser [a] -> AsParser [a] -> AsParser [[a]]
sepByI1 p sep = do{ x <- p
                 ; xs <- many (do{ s <- sep; i<- p; return (s++i)})
                 ; return (x:xs)
                 }

sepByCI1 :: AsParser CToken -> AsParser CToken -> AsParser [CToken]
sepByCI1 p sep = do{ x <- p
                 ; xs <- many (do{ s <- sep; i<- p; return [s,i]})
                 ; return (x:(concat xs))
                 }

sepEndByI1 :: AsParser [a] -> AsParser [a] -> AsParser [[a]]
sepEndByI1 p sep = do{ x <- sepEndBy1 p sep
                     ; s <- sep
                     ; return $ x++[s]
                     }

ident' = do{ n <- sepByI1 id' (op' "."); return (concat n)}
-- ident : qualified identifier
ident = mylexeme $ ident'
-- sident : qualified identifier with possible * at end
sident = mylexeme $ try( do{ n <- sepEndByI1 id' (op' "."); o <- op' "*"; return $ (concat n) ++ o}) <|> ident'
-- nident : identifier qualified with namespace
nident = mylexeme $ do{ q<- try(do{ n'<- ident'; c <- op' "::"; return $ n'++c }) <|> return []; n <- ident'; return $ q++n } 

anytok = mylexeme $ anytok'

maybeSemi = optionMaybe $ op ";"
