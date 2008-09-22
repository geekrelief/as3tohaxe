module ActionhaXe.Prim where

import ActionhaXe.Lexer
import Text.Parsec
import Text.Parsec.Prim
import Data.Map (Map)

type Name = String
type TList = [Token]
type CToken = (TList, TList) -- compound token with a list for an entity, whitespace

type Semi = Maybe CToken

type AsState = [(Map AsDef AsDefInfo)] 
type AsParser = Parsec TList AsState

data AsType = AsTypeVoid
            | AsTypeBoolean
            | AsTypeNumber
            | AsTypeString
            | AsTypeDynamic
            | AsTypeObject
            | AsTypeArray AsType
            | AsTypeUserDefined CToken
            | AsTypeUnknown
    deriving (Show)


-- Symbol Lookup key
data AsDef = DefPackage   Name
           | DefClass     Name
           | DefInterface Name
           | DefFunction  Name  -- anonymous functions are not referenced
           | DefVar       Name  -- can be for constants too
           | DefNamespace Name
    deriving (Show)

type Attribute = String 
-- Symbol Lookup value
data AsDefInfo = DiNone             --
               | DiClass    [Attribute] (Maybe AsDef) (Maybe [AsDef]) -- attributes, extends, implements
               | DiFunction [Attribute]
               | DiVar      AsType
    deriving (Show)



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

mylexeme p = do{ x <- p; w <- whiteSpace; return (x, w)}

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

maybeSemi = optionMaybe $ op ";"
