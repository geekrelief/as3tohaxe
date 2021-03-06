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
module ActionhaXe.Data where

import ActionhaXe.Lexer
import ActionhaXe.CLArgs

import Text.Parsec
import Text.Parsec.Prim

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree
import Data.Generics -- not Haskel 98

-- data
showd :: CToken -> String
showd x = foldr (\t s -> (tokenItemS t) ++ s) "" (fst x)

-- whitespace includes comments
showw :: CToken -> String
showw x = foldr (\t s -> (tokenItemS t) ++ s) "" (snd x)

-- whitespace filters comments
showws :: CToken -> String
showws x = foldr (\t s -> (tokenItemS t) ++ s) "" (fst (span (\(s, tok) -> case tok of { TokenWhite _ -> True; TokenNl _ -> True; otherwise -> False;}) (snd x)))

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

-- splits the space from the left and right
splitLR :: String -> [String]
splitLR x = [l, m, r]
    where (l, nl) = span (\c -> isSpace c || c == '\n') x
          (r') = span (\c -> isSpace c || c == '\n') $ reverse nl
          (m, r) = (reverse $ snd r', reverse $ fst r')

type Name = String
type TList = [Token]
type CToken = (TList, TList) -- compound token with a list for an entity, whitespace

type Semi = Maybe CToken

data PrimaryE = PEThis CToken                     -- this
              | PEIdent CToken                    -- identifier
              | PELit CToken                      -- literal: null, boolean, numeric, string, not regular expression or xml since those don't have operations on them outside of class methods
              | PEArray ArrayLit                  -- array literal
              | PEObject ObjectLit                -- {, maybe [[property : assignE],[,]] , }
              | PEXml CToken
              | PEFunc FuncE
              | PEParens CToken ListE CToken  -- (, List of expressions , )
    deriving (Show, Data, Typeable)

data ListE = ListE [(AssignE, (Maybe CToken))] 
    deriving (Show, Data, Typeable)

data ArrayLit = ArrayLitC CToken (Maybe Elision) CToken  -- [, maybe commas, ]
              | ArrayLit  CToken ElementList CToken      -- [, element list, ]
    deriving (Show, Data, Typeable)

data ElementList = El (Maybe Elision) AssignE [EAE] (Maybe Elision)
    deriving (Show, Data, Typeable)

data EAE = EAE Elision AssignE
    deriving (Show, Data, Typeable)

data Elision = Elision [CToken]  -- commas possible separated by space in a list
    deriving (Show, Data, Typeable)

data ObjectLit = ObjectLit CToken (Maybe PropertyList) CToken deriving (Show, Data, Typeable)

data PropertyList = PropertyList [(CToken, CToken, AssignE, (Maybe CToken))]  deriving (Show, Data, Typeable) -- [propertyName, :, assignE, maybe ',']

data FuncE = FuncE CToken (Maybe CToken) Signature BlockItem deriving (Show, Data, Typeable) -- function, maybe ident, signature, block

data Arguments = Arguments CToken (Maybe ListE) CToken
    deriving (Show, Data, Typeable)

data SuperE = SuperE CToken (Maybe Arguments)
    deriving (Show, Data, Typeable)

data PostFixE = PFFull FullPostFixE (Maybe CToken)
              | PFShortNew ShortNewE (Maybe CToken)
    deriving (Show, Data, Typeable)

data FullPostFixE = FPFPrimary PrimaryE [FullPostFixSubE]
                  | FPFFullNew FullNewE [FullPostFixSubE]
                  | FPFSuper SuperE PropertyOp [FullPostFixSubE]
                  | FPFInc PostFixE CToken
                  | FPFDec PostFixE CToken
    deriving (Show, Data, Typeable)

data FullPostFixSubE = FPSProperty PropertyOp
                     | FPSArgs Arguments 
                     | FPSQuery QueryOp
    deriving (Show, Data, Typeable)

data FullNewE = FN CToken FullNewE Arguments
              | FNPrimary PrimaryE [PropertyOp]
              | FNSuper SuperE [PropertyOp]
    deriving (Show, Data, Typeable)

data ShortNewE = SN CToken ShortNewSubE
    deriving (Show, Data, Typeable)

data ShortNewSubE = SNSFull FullNewE
                  | SNSShort ShortNewE
    deriving (Show, Data, Typeable)

data PropertyOp = PropertyOp CToken CToken  -- . , identifier
                | PropertyB CToken ListE CToken -- [ list expression ]
    deriving (Show, Data, Typeable)

data QueryOp = QueryOpDD CToken CToken
             | QueryOpD CToken CToken ListE CToken
    deriving (Show, Data, Typeable)

data UnaryE = UEPrimary PostFixE
            | UEDelete CToken PostFixE
            | UEVoid CToken PostFixE
            | UETypeof CToken PostFixE
            | UEInc CToken PostFixE
            | UEDec CToken PostFixE
            | UEPlus CToken UnaryE
            | UEMinus CToken UnaryE
            | UEBitNot CToken UnaryE
            | UENot CToken UnaryE
    deriving (Show, Data, Typeable)

data AritE = AEUnary UnaryE
           | AEBinary CToken AritE AritE
    deriving (Show, Data, Typeable)

data RegE = RegE CToken [CToken] CToken (Maybe CToken)
    deriving (Show, Data, Typeable)

data CondE = CondE AritE (Maybe (CToken, AssignE, CToken, AssignE))
           | CondRE RegE
    deriving (Show, Data, Typeable)

data NAssignE = NAssignE AritE (Maybe (CToken, NAssignE, CToken, NAssignE))
    deriving (Show, Data, Typeable)

data AssignE = ACond CondE
             | AAssign PostFixE CToken AssignE
             | ACompound PostFixE CToken AssignE
             | ALogical PostFixE CToken AssignE
    deriving (Show, Data, Typeable)

data BlockItem =  Tok        CToken
                | Expr       AssignE
                | Block      CToken [BlockItem] CToken
                | ImportDecl CToken CToken Semi  -- import identifier ;
                | ClassDecl  [CToken] CToken CToken (Maybe (CToken, CToken)) (Maybe (CToken, [(CToken, Maybe CToken)])) BlockItem -- attributes, class, identifier, maybe (extends, idn), maybe (implements [(idn, maybe',')]), body
                | Interface  [CToken] CToken CToken (Maybe (CToken, [(CToken, Maybe CToken)])) BlockItem -- public | internal, interface, identifier, maybe (extends [(idn, maybe ',')], body
                | MethodDecl [CToken] CToken (Maybe CToken) CToken Signature (Maybe BlockItem) -- attributes, function, maybe get/set, identifier, Signature, body
                | VarS       [CToken] CToken VarBinding [(CToken, VarBinding)] -- maybe attributes, var, varbinding, [(',', varbinding)]
                | ForS       CToken CToken (Maybe ForInit) CToken (Maybe ListE) CToken (Maybe ListE) CToken BlockItem -- for( ? ; ? ; ?) {}
                | ForInS     CToken (Maybe CToken) CToken ForInBinding CToken ListE CToken BlockItem -- for, maybe each, ( forinbinding in listE ) {}
                | Metadata   Metadata
    deriving (Show, Data, Typeable)

data Metadata = MDSwf [(CToken, CToken)]
              | MD    CToken CToken [CToken] CToken
    deriving (Show, Data, Typeable)

data ForInit = FIListE ListE
             | FIVarS  BlockItem
    deriving (Show, Data, Typeable)

data ForInBinding = FIBPostE PostFixE
                  | FIBVar CToken VarBinding  -- var | const , varbinding
    deriving (Show, Data, Typeable)

data VarBinding = VarBinding   CToken (Maybe (CToken, AsType)) (Maybe (CToken, AssignE)) --varbinding with Type-- identifier, (maybe (:, datatype)), (maybe (=, assignE))
    deriving (Show, Data, Typeable)

data Signature =  Signature  CToken [Arg] CToken (Maybe (CToken, AsType)) -- left paren, arguments, right paren, :,  return type
    deriving (Show, Data, Typeable)

data Arg = Arg CToken CToken AsType (Maybe (CToken, AssignE)) (Maybe CToken) -- arg name, :, type, maybe default value, maybe comma
         | RestArg CToken CToken (Maybe (CToken, AsType)) -- ..., name
    deriving (Show, Data, Typeable)

data Package = Package CToken CToken (Maybe CToken) BlockItem -- whitespace, package, maybe name, block
    deriving (Show, Data, Typeable)

data Ast = AS3Program Package AsState
         | AS3Directives [BlockItem] AsState
    deriving (Show, Data, Typeable)

type AsParser = Parsec TList AsState

--- type and state structures

data AsType = AsType CToken
            | AsTypeRest
            | AsTypeUser CToken
            | AsTypeUnknown
    deriving (Show, Eq, Ord, Data, Typeable)
           
-- Symbol Lookup key
data AsDef = DefPackage   Name
           | DefClass     Name
           | DefInterface Name
           | DefFunction  Name  -- anonymous functions are not referenced
           | DefVar       Name  -- can be for constants too
           | DefNamespace Name  
           | DefNone
    deriving (Show, Eq, Ord, Data, Typeable)

type Attribute = String
-- Symbol Lookup value
data AsDefInfo = DiNone             --
               | DiClass    [Attribute] (Maybe AsDef) (Maybe [AsDef]) -- attributes, extends, implements
               | DiFunction [Attribute] 
               | DiVar      AsType
    deriving (Show, Eq, Ord, Data, Typeable)

type AsDefTuple = (AsDef, AsDefInfo)

data AsStateEl = AsStateEl { sid::Int, scope::Map AsDef AsDefInfo }
    deriving (Show, Data, Typeable)

data AsState = AsState{ conf::Conf, filename::String, outfile::String, curId::Int, flags::Map String String, accessors::Map String (AsType, Bool, Bool), initMembers::[String], path::[Int], scopes::Tree AsStateEl }
    deriving (Show, Data, Typeable)

initState :: AsState
initState = AsState{ conf = ConfNone, filename = "", outfile = "", curId = 0, path = [0], flags = Map.empty, accessors = Map.empty, initMembers = [], scopes = newScope 0}

getProperty name = 
    do x <- getState
       let a = accessors x
       let def = (AsTypeUnknown, False, False)
       case Map.lookup name a of
           Nothing   -> do let a' = Map.insert name def a
                           let x' = x{ accessors = a' } 
                           setState x'
                           return def
           Just prop -> return prop

setProperty name prop =
    do x <- getState
       let a = accessors x
       let a' = Map.insert name prop a
       let x' = x{accessors = a'}
       setState x'

addGetter name astype = 
    do prop@(t, g, s) <- getProperty name
       setProperty name (astype, True, s)

addSetter name astype =
    do prop@(t, g, s) <- getProperty name
       setProperty name (astype, g, True)

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

storeProperty name (Just acc) s@(Signature l a r o) =
    do{ case showd acc of
            "get" -> addGetter (showd name) $ ret o
            "set" -> addSetter (showd name) $ arg $ head a
      }
    where ret (Just (_, t)) = t
          arg (Arg _ _ t _ _) = t

storeProperty name Nothing s = return ()

storeVar :: CToken -> AsType -> AsParser ()
storeVar v t = updateSymbol (DefVar (showd v), DiVar t)
