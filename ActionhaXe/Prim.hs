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
module ActionhaXe.Prim where

import ActionhaXe.Lexer
import ActionhaXe.Data
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Pos

mytoken :: (Token -> Maybe a) -> AsParser a
mytoken test = token showTok posFromTok testTok
    where showTok (pos, t)    = show t
          posFromTok a@(pos, t) = newPos (tokenSource a) (tokenLine a) (tokenCol a)
          testTok t         = test t

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

kw' k = mytoken $ \t -> case tokenItem t of
                           TokenKw k'| k == k' -> Just [t]
                           _ -> Nothing

op' o = mytoken $ \t -> case tokenItem t of
                           TokenOp o' | o == o' -> Just [t]
                           _ -> Nothing

xml' = mytoken $ \t -> case tokenItem t of
                           TokenXml x -> Just [t]
                           _ -> Nothing

idn = mylexeme $ id'
num = mylexeme $ num'
str = mylexeme $ str'
xml = mylexeme $ xml'

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
sepEndByI1 p sep = do{ x <- sepByI1 p sep
                     ; s <- sep
                     ; return $ x++[s]
                     }

ident' = do{ n <- sepByI1 id' (op' "."); return (concat n)}
-- ident : qualified identifier
ident = mylexeme $ ident'
-- sident : qualified identifier with possible * at end
sident = mylexeme $ try( do{ n <- id'; ns <- many(do{ d <- op' "."; n <- (id' <|> op' "*"); return $ d++n}); return $ n ++ concat ns}) <|> ident'
--sident = mylexeme $ try( do{ n <- mid' "event"; p <- op' "."; o <- op' "*"; return $ n ++ p ++ o}) <|> ident'
-- nident : identifier qualified with namespace
nident = mylexeme $ do{ q<- try(do{ n'<- ident'; c <- op' "::"; return $ n'++c }) <|> return []; n <- ident'; return $ q++n } 

anytok = mylexeme $ anytok'

maybeSemi = optionMaybe $ op ";"
