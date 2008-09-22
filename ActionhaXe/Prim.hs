module ActionhaXe.Prim where

import ActionhaXe.Lexer
import Text.Parsec
import Text.Parsec.Prim
import Data.Map (Map)

type TList = [Token]
type Identifier = (TList, TList)
type AsState = [(Map String AsType)] -- identifier and type -- type can change as file is parsed
type AsParser = Parsec TList AsState

data AsType = AsTypeVoid
            | AsTypeBoolean
            | AsTypeNumber
            | AsTypeString
            | AsTypeDynamic
            | AsTypeObject
            | AsTypeArray AsType
            | AsTypeUserDefined 
            | AsTypeUnknown
    deriving (Show, Eq)

data CToken = CToken [Token] [Token]  -- compound token with a list for an entity and whitespace
    deriving (Show, Eq)

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

mylexeme p = do{ x <- p; w <- whiteSpace; return $ CToken x w}

num' = mytoken $ \t -> case tokenItem t of
                          TokenNum x -> Just t
                          _ -> Nothing

id' = mytoken $ \t -> case tokenItem t of
                         TokenIdent x -> Just [t]
                         _ -> Nothing

str' = mytoken $ \t -> case tokenItem t of
                         TokenString x -> Just t
                         _ -> Nothing

reg' = mytoken $ \t -> case tokenItem t of
                          TokenRegex x -> Just t
                          _ -> Nothing

kw' k = mytoken $ \t -> case tokenItem t of
                           TokenKw k'| k == k' -> Just [t]
                           _ -> Nothing

op' o = mytoken $ \t -> case tokenItem t of
                           TokenOp o' | o == o' -> Just [t]
                           _ -> Nothing

xml' = mytoken $ \t -> case tokenItem t of
                           TokenXml x -> Just t
                           _ -> Nothing

kw k = mylexeme $ kw' k
op o = mylexeme $ op' o

sepByI1 :: AsParser [a] -> AsParser [a] -> AsParser [a]
sepByI1 p sep = do{ x <- p
                 ; xs <- many (do{ s <- sep; i<- p; return (s++i)})
                 ; return $ concat (x:xs)
                 }

ident' = do{ n <- sepBy1 id' (op' "."); return (concat n)}
-- ident : qualified identifier
ident = mylexeme $ ident'
-- sident : qualified identifier with possible * at end
sident = mylexeme $ try( do{ n <- sepEndBy1 id' (op' "."); o <- op' "*"; return $ (concat n) ++ o}) <|> ident'
-- nident : identifier qualified with namespace
nident = mylexeme $ do{ q<- try(do{ n'<- ident'; c <- op' "::"; return $ n'++c }) <|> return []; n <- ident'; return $ q++n } 

anytok = mylexeme $ anytok'
