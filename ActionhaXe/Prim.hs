module ActionhaXe.Prim where

import ActionhaXe.Lexer
import Text.Parsec
import Text.Parsec.Prim

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree

type Name = String
type TList = [Token]
type CToken = (TList, TList) -- compound token with a list for an entity, whitespace

-- data
showd :: CToken -> String
showd x = foldr (\t s -> (tokenItemS t) ++ s) "" (fst x)

-- whitespace
showw :: CToken -> String
showw x = foldr (\t s -> (tokenItemS t) ++ s) "" (snd x)

-- both data and whitespace
showb :: CToken -> String
showb x = foldr (\t s -> (tokenItemS t) ++ s) "" ((fst x)++(snd x))

-- the whitespace after the first newline
shown :: CToken -> String
shown x = tailAtN $ snd $ break (== '\n') $ foldr (\t s -> (tokenItemS t) ++ s) "" (snd x)
    where tailAtN [] = ""
          tailAtN t  = tail t

showl :: [CToken] -> String
showl xs = foldr (\t s -> showb t ++ s) "" xs
           

type AsParser = Parsec TList AsState

data AsType = AsType CToken
            | AsTypeRest
            | AsTypeUser CToken
    deriving (Show, Eq, Ord)
           
-- Symbol Lookup key
data AsDef = DefPackage   Name
           | DefClass     Name
           | DefInterface Name
           | DefFunction  Name  -- anonymous functions are not referenced
           | DefVar       Name  -- can be for constants too
           | DefNamespace Name  
           | DefNone
    deriving (Show, Eq, Ord)

type Attribute = String
-- Symbol Lookup value
data AsDefInfo = DiNone             --
               | DiClass    [Attribute] (Maybe AsDef) (Maybe [AsDef]) -- attributes, extends, implements
               | DiFunction [Attribute] 
               | DiVar      AsType
    deriving (Show, Eq, Ord)

type AsDefTuple = (AsDef, AsDefInfo)

data AsStateEl = AsStateEl { sid::Int, scope::Map AsDef AsDefInfo }
    deriving (Show)
data AsState = AsState{ curId::Int, flags::Map String String, path::[Int], scopes::Tree AsStateEl }
    deriving (Show)

initState :: AsState
initState = AsState{ curId = 0, path = [0], flags = Map.empty, scopes = newScope 0}

enterScope :: AsParser ()
enterScope = do x <- getState
                let c = (1 +) $ curId x
                let p = (c:) $ path x
                let s = scopes x
                let s' = addScope (reverse p) s
                let x' = x{ curId = c, path = p, scopes = s'}
                setState x'
                return ()

newScope :: Int -> Tree AsStateEl
newScope i = Node{ rootLabel = AsStateEl{ sid = i, scope = Map.empty}, subForest = []}

addScope :: [Int] -> Tree AsStateEl -> Tree AsStateEl
addScope (p:ps) t = t{ subForest = addScope' ps (subForest t)}

addScope' :: [Int] -> Forest AsStateEl -> Forest AsStateEl
addScope' (p:[]) [] = [newScope p]
addScope' path@(p:ps) (t:ts) = if p == (sid $ rootLabel t) then ((addScope path t):ts) else (t:(addScope' path ts))

exitScope :: AsParser ()
exitScope = do x <- getState
               let p = tail $ path x
               let x' = x{path = p}
               setState x'
               return ()

lookupSymbol :: AsParser AsDef
lookupSymbol = return DefNone

updateSymbol :: AsDefTuple -> AsParser ()
updateSymbol d = do x <- getState
                    x' <- updateS d x
                    setState x'

updateS :: AsDefTuple -> AsState -> AsParser AsState
updateS d x = do let p = path x
                 let s = scopes x
                 let s' = traverseS (reverse p) s d
                 return x{scopes = s'}

traverseS :: [Int] -> Tree AsStateEl -> AsDefTuple -> Tree AsStateEl
traverseS (p:[]) t (d, di) = if p == (sid $ rootLabel t) then t{ rootLabel = AsStateEl { sid = (sid $ rootLabel t), scope = (Map.insert d di (scope $ rootLabel t))}} else fail $ "\n\n--Error: in updating state "++(show p)++" "++(show t)
traverseS (p:ps) t dt = if p == (sid $ rootLabel t) then t{ subForest = traverseS' ps (subForest t) dt }  else t

-- check the subtrees
traverseS' :: [Int] -> Forest AsStateEl -> AsDefTuple -> Forest AsStateEl
traverseS' path@(p:[]) [t] dt = [traverseS path t dt]
traverseS' path@(p:ps) (t:ts) dt = if p == (sid $ rootLabel t) then ((traverseS path t dt):ts) else (t:(traverseS' path ts dt))

storePackage :: Maybe CToken -> AsParser ()
storePackage p = case p of
                     Just x -> updateSymbol (DefPackage (showd x), DiNone)
                     Nothing -> updateSymbol (DefPackage "//noname", DiNone)

storeClass :: CToken -> AsParser ()
storeClass c = updateSymbol (DefClass (showd c), DiNone)

storeMethod :: CToken -> AsParser ()
storeMethod m = updateSymbol (DefFunction (showd m), DiNone)

storeVar :: CToken -> AsType -> AsParser ()
storeVar v t = updateSymbol (DefVar (showd v), DiVar t)

-- basic parsers

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
