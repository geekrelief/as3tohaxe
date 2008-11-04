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
-- Translate an Actionscript 3 AST to haXe and hxml for Flash 10

module ActionhaXe.Translator where

import ActionhaXe.Lexer
import ActionhaXe.Data
import ActionhaXe.Prim
import ActionhaXe.Parser
import ActionhaXe.CLArgs
import qualified System.Console.ParseArgs as PA

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Data.Foldable (foldlM, foldrM)
import Data.List (intercalate)
import Data.Char (toUpper, isAlphaNum)
import Data.Generics -- not Haskell 98

import Text.Regex

-- flags
mainPackage = "mainPackage"
fpackage  = "packageName"
fclass = "className"
fclassAttr = "classAttr"

updateFlag flag val = do st <- get
                         put st{flags = Map.insert flag val (flags st)}

deleteFlag flag = do st <- get
                     put st{flags = Map.delete flag (flags st)}

getFlag :: String -> StateT AsState IO String
getFlag flag = do st <- get
                  if Map.member flag $ flags st
                      then do mval <- Map.lookup flag $ flags st
                              return mval
                      else return ""

insertInitMember output = do st <- get
                             put st{initMembers = (output:(initMembers st))}
                             st' <- get
                             return ()

getMembers = do st <- get
                let ret = reverse $ initMembers st
                put st{initMembers = []}
                return ret

getCLArg f = do st <- get
                return $ PA.gotArg (confArgs (conf st)) f

maybeEl f i = maybe "" (\m -> f m) i

cleanup s = subRegex (mkRegex "\\$") s "_S_"

--translateAs3Ast :: Ast -> StateT AsState IO String
translateAs3Ast p = 
    case p of 
        AS3Program    x st -> program x >>= return
        AS3Directives x st -> directives x >>= return

directives ds = foldlM (\str i -> do{s <- directive i; return $ str++s}) "" ds
    where directive d = case d of
                            Tok t                       -> do{ t' <- tok t; return $ cleanup t'}
                            ImportDecl _ _ _            -> return $ cleanup $ importDecl d
                            Metadata m                  -> do{ m' <- metadata m; return $ cleanup m'}
                            MethodDecl _ _ _ _ _ _      -> do{ d' <- methodDecl d; return $ cleanup d'}
                            VarS _ _ _ _                -> do{ d' <- memberVarS d; return $ cleanup d'}
                            _                           -> return "--unexpected directive--"

--program :: Package -> StateT AsState IO String
program (Package w p n b) = do case n of
                                 Just ntok -> do{ updateFlag fpackage $ showd ntok ; x <- packageBlock b; return $ cleanup $ showb w ++ showd p ++" "++ showd ntok ++ ";" ++ showw ntok ++ x}
                                 Nothing   -> do{ updateFlag fpackage mainPackage; x <-packageBlock b; return $ cleanup $ showb w ++ showw p ++ x}

packageBlock (Block l bs r)  = do 
    bi <- foldlM (\s b -> do{ x <- packageBlockItem b; return $ s ++ x} ) "" bs 
    return $ showw l ++ bi

classBlock (Block l bs r)  = do 
    x <- get
    let a = accessors x
    let al = Map.toList a
    props <- foldlM (\str (k, (t, g, s)) -> do{ t' <- datatype t
                                              ; return $ str ++ showws l ++ "public var " ++ k ++ "(" 
                                                         ++ (if g then "get"++ [toUpper $ head k] ++ tail k else "null") ++ ", "
                                                         ++ (if s then "set"++ [toUpper $ head k] ++ tail k else "null") 
                                                         ++ ") : " ++ t' ++ ";"
                                              }
                     ) "" al
    bi <-  foldlM (\s b -> do{ x <- classBlockItem b; return $ s ++ x} ) "" bs 
    return $ showd l ++ props ++ showw l ++ bi ++ showb r

interfaceBlock (Block l bs r)  = do 
    bi <-  foldlM (\s b -> do{ x <- interfaceBlockItem b; return $ s ++ x} ) "" bs 
    return $ showb l ++ bi ++ showb r

blockItemFold bs = foldlM (\s b -> do{ x <- blockItem b; return $ s ++ x} ) "" bs 

block (Block l bs r)  = do 
--    bi <-  foldlM (\s b -> do{ x <- blockItem b; return $ s ++ x} ) "" bs 
    bi <- blockItemFold bs
    return $ showb l ++ bi ++ showb r

constructorBlock (Block l bs r) = do 
    --bi <-  foldrM (\b s -> do{ x <- blockItem b; return $ x ++ s} ) "" bs 
    bi <-  blockItemFold bs
    let spacebreak = break (\c -> c == '\n') ( reverse $ showw l)
    let i =  reverse $ fst spacebreak
    let nl = if length (snd spacebreak) > 1 && (snd spacebreak)!!1 == '\r' then "\r\n" else "\n"
    x <- getMembers
    let init = if length x > 0
                   then nl++i++ intercalate (nl++i) x ++ nl ++ i
                   else ""
    return $ showb l ++ init ++ bi ++ showb r

packageBlockItem b = 
    do x <- case b of
                Tok t                       -> tok t >>= return
                ImportDecl _ _ _            -> return $ importDecl b
                ClassDecl _ _ _ _ _ _       -> classDecl b >>= return
                Interface _ _ _ _ _         -> interface b >>= return
                Metadata m                  -> metadata m >>= return
                _                           -> return ""
       return x

classBlockItem b = 
    do x <- case b of
                Tok t                       -> tok t >>= return
                MethodDecl _ _ _ _ _ _      -> methodDecl b >>= return
                VarS _ _ _ _                -> memberVarS b >>= return
                Metadata m                  -> metadata m >>= return
                _                           -> return $ show b
       return x

interfaceBlockItem b = 
    do x <- case b of
                Tok t                       -> tok t >>= return
                MethodDecl _ _ _ _ _ _      -> imethodDecl b >>= return
                Metadata m                  -> metadata m >>= return
                _                           -> return $ show b
       return x

blockItem b = 
    do x <- case b of
                Tok t                       -> tok t >>= return
                Block _ _ _                 -> block b >>= return
                VarS _ _ _ _                -> varS b >>= return
                ForS _ _ _ _ _ _ _ _ _      -> forS b >>= return
                Expr _                      -> expr b >>= return
                Metadata m                  -> metadata m >>= return
                _                           -> return ""
       return x

tok t = do let x = showb t
           f <- getFlag fpackage
           return x

metadata m = case m of
                 MD l t x r -> do{ return $ "/*" ++ showb l ++ showb t ++ showl x++ showd r ++ "*/" ++ showw r}
                 MDSwf attr -> do{ o <- swfheader attr ["400", "300", "30", "FFFFFF"]
                                 ; st <- get
                                 ; let outclass = reverse $ drop 3 $ reverse $ filename st
                                 ; liftIO $ writeFile ((outfile st) ++ "ml") ("-swf "++outclass++".swf\n-main "++outclass++"\n-swf-version 10\n-swf-header " ++ o)
                                 ; return ""
                                 }
    where swfheader ((k, v):as) [w, h, f, b] = 
              case showd k of 
                  "width"            -> swfheader as [init.tail $ showd v, h, f, b]
                  "height"           -> swfheader as [w, init.tail $ showd v, f, b]
                  "frameRate"        -> swfheader as [w, h, init.tail $ showd v, b]
                  "backgroundColor"  -> swfheader as [w, h, f, tail.init.tail $ showd v]
          swfheader [] header = return $ intercalate ":" header

importDecl (ImportDecl i n s) = foldr (\t s -> showb t ++ s) "" [i,n] ++ maybeEl showb s  -- look up and adjust

classDecl (ClassDecl a c n e i b) = do
    updateFlag fclass $ showd n
    updateFlag fclassAttr $ publicAttr a
    x <- classBlock b
    let e' = maybe [] (\(k, c) -> if showd c == "Object" then [] else [showb k ++ showd c] ) e
    let i' = maybe [] (\(ic, cs) -> map (\(x, co) -> showb ic ++ showd x) cs ) i
    let i'' = i' ++ if "dynamic" `elem` map (\a' -> showd a') a then ["implements Dynamic<Dynamic>"] else [""]
    let ei = intercalate ", " $ filter (\x -> length x > 0) $ e'++i''
    return $ namespace a ++ showb c ++ showb n ++ ei ++ " " ++ x 
    where publicAttr as = if "public" `elem` map (\a -> showd a) as then "public" else "private"

interface (Interface a i n e b) = do
    x <- interfaceBlock b
    let e' = maybe "" (\(e, c) -> "implements "++showd c ) e
    return $ namespace a ++ showb i ++ showb n ++ e' ++ x

methodDecl (MethodDecl a f ac n s b) = do 
    packageName <- getFlag fpackage
    className <- getFlag fclass
    classAttr <- getFlag fclassAttr
    if packageName == mainPackage && className == (showd n) && classAttr == "public"
        then do{ x <- maybe (return "") block b; return $ "static " ++ showb f ++ "main() "++ x }
        else if className == (showd n)
                 then do{ x <- maybe (return "") constructorBlock b; s' <- signatureArgs s; return $ namespace a ++ showb f ++ "new"++showw n ++ s' ++ x }
                 else do st <- get
                         let accMap = accessors st
                         if Map.member (showd n) accMap
                              then do (t, _, _) <- Map.lookup (showd n) accMap
                                      let arg = getArg s
                                      x <- maybe (return "" ) (accblock arg ac) b
                                      acc' <- accessor ac n s t
                                      return $ namespace a ++ showb f ++ acc' ++ x 
                              else do x <- maybe (return "") block b
                                      s' <- signature s
                                      return $ namespace a ++ showb f ++ showb n ++ s' ++ x
    where accessor ac name s@(Signature l args r ret) t = 
              case ac of
                  Just x -> do{ a <- showArgs args
                              ; t' <- datatypet t
                              ; return $ showd x ++ [toUpper $ head $ showd name] ++ tail (showb name) ++ showb l ++ a ++ showd r ++ ":" 
                                ++ fst t' ++ (case ret of { Just (c, t) -> snd t'; Nothing -> showw r})
                              }
                  Nothing -> do{ s' <- signature s; return $ showb name ++ s'}
          accblock arg ac (Block l bs r) = 
              do let ts = case ac of { Just x -> if showd x == "set" then "\treturn "++arg++";"++ init(showw l) else ""; Nothing -> ""}
                 --bi <-  foldlM (\s b -> do{ x <- blockItem b; return $ s ++ x} ) "" bs
                 bi <-  blockItemFold bs
                 return $ showb l ++ bi ++ ts ++ showb r
          getArg (Signature l (a@(Arg n c t md mc):as) r ret) = showd n
          getArg (Signature l [] r ret) = ""


imethodDecl (MethodDecl a f ac n s b) = do 
    s' <- signature s
    return $ attr a ++ showb f ++ showb n ++ s'
    where attr as = concat $ map (\attr -> case (showd attr) of { "internal" -> "private" ++ showw attr; x -> showb attr }) as


signatureArgs (Signature l args r ret) = do{ a <- showArgs args
                                           ; return $ showb l ++ a ++ showb r
                                           }

rettype ret = case ret of
                  Just (c, t) -> do{ t' <- datatype t; return $ showb c ++ t'}
                  Nothing     -> return ""

signature (Signature l args r ret) = do{ a <- showArgs args
                                       ; ret' <- rettype ret
                                       ; return $ showb l ++ a ++ showb r ++ ret'
                                       }

showArgs as = do{ as' <- mapM showArg as; return $ concat as'}
    where showArg (Arg n c t i mc) = do{ i' <- maybe (return "") (\(o, e) -> do{ e' <- assignE e; return $ showb o ++ e'}) i
                                       ; t' <- datatypeiM t i
                                       ; return $ (case i of{ Just d  -> "?"; Nothing -> ""}) ++ showb n ++ showb c ++ t' ++ i' ++ maybeEl showb mc
                                       }
          showArg (RestArg o n t) = do{ return $ showd n ++ ":Array<Dynamic>"}

memberVarS (VarS ns k v vs) = do 
    if elem "static" (map (\n -> showd n) ns)
        then do{ v' <- varBinding v False
               ; vs' <- foldlM (\s (c, x) -> do{ x' <- varBinding x False; return $ s ++ showb c ++ x'}) "" vs
               ; let inl = if hasPrimitive v && length vs == length (filter (\(c, v) -> hasPrimitive v) vs) then "inline " else ""
               ; return $ inl ++ namespace ns ++ "var" ++ showw k ++ v' ++ vs'}
        else do{ v' <- varBinding v True; vs' <- foldlM (\s (c, x) -> do{ x' <- varBinding x True; return $ s ++ showb c ++ x'}) "" vs; return $ namespace ns ++ "var" ++ showw k ++ v' ++ vs'}

hasPrimitive = everything (||) (False `mkQ` hasPrimitive')

hasPrimitive' (TokenNum _) = True
hasPrimitive' (TokenString _) = True
hasPrimitive' (TokenKw "true") = True
hasPrimitive' (TokenKw "false") = True
hasPrimitive' _ = False

varS (VarS ns k v vs) = do{ v' <- varBinding v False; vs' <- foldlM (\s (c, x) -> do{ x' <- varBinding x False; return $ s++ showb c ++x' }) "" vs; return $ namespace ns ++ "var" ++ showw k ++ v' ++ vs'}

varBinding :: VarBinding -> Bool -> StateT AsState IO String
varBinding (VarBinding n dt i) initMember = 
    do{ d' <- case dt of
                  Just (c, t) -> do{ d <- datatypeiM t i; return $ showb c ++ d}
                  Nothing -> case i of -- try to determine type from initializer
                                 Just (o, e) -> do let e' = getType e
                                                   case e' of
                                                       Just t -> return $ ":" ++ t ++ showw n -- set type to initializer's type
                                                       Nothing -> return $ ":Dynamic" ++ showw n -- can't determine type
                                 Nothing -> return $ ":Dynamic" ++ showw n -- no datatype, no initializer
      ; i' <- maybe (return "") (\(o, e) -> do{ e' <- assignE e; return $ showb o ++ e'}) i; 
      ; if i' /= "" && initMember
            then do{ insertInitMember $ showb n ++ (if last(showb n) == ' ' then "" else " ") ++ i' ++ ";"
                   ; return $ showd n ++ d' }
            else return $ showd n ++ d' ++ i'
      }



getType = everything orElse ((Nothing `mkQ` getTypeTokenNum) `extQ` getTypeTokenType)

getTypeTokenNum (TokenDouble x) = Just "Float"
getTypeTokenNum (TokenInteger x) = Just "Int"
getTypeTokenNum (TokenHex x) = Just "Int"
getTypeTokenNum (TokenOctal x) = Just "Int"
getTypeTokenType (TokenString x) = Just "String"
getTypeTokenType (TokenKw "true") = Just "Bool"
getTypeTokenType (TokenKw "false") = Just "Bool"
getTypeTokenType _ = Nothing

namespace ns = concat $ map (\attr -> (case showd attr of { "dynamic" -> ""
                                                          ; "final" -> ""
                                                          ; "protected" -> ""
                                                          ; "internal" -> "private" ++ showw attr
                                                          ; "public" -> ""
                                                          ; _ -> showb attr 
                                                          }
                                      ) ) ns

datatypet d = do{ d' <- datatype d; return $ span (\c -> isAlphaNum c || c =='>' || c == '<') d'}

datatype :: AsType -> StateT AsState IO String
datatype d = 
    case d of
        AsType n -> do d' <- (case (showd n) of 
                                  "void"    -> return "Void"
                                  "Boolean" -> return "Bool"
                                  "uint"    -> return "UInt"
                                  "int"     -> return "Int"
                                  "Number"  -> do ni <- getCLArg NumberToInt
                                                  if ni
                                                       then return "Int"
                                                       else return "Float"
                                  "String"  -> return "String"
                                  "*"       -> return "Dynamic"
                                  "Object"  -> return "Dynamic"
                                  "Function"-> return "Dynamic"
                                  "Array"   -> return "Array<Dynamic>"
                                  "XML"     -> return "XML"
                                  "RegExp"  -> return "EReg"
                              )
                       return $ d' ++ showw n
        AsTypeRest -> return "Array<Dynamic>"
        AsTypeUser n -> return $ showb n

datatypeiM d Nothing = datatype d
datatypeiM d i = 
    case d of
        AsType n -> do{ r <- case (showd n) of
                            "void"    -> return "Void"
                            "Boolean" -> return "Bool"
                            "uint"    -> return "UInt"
                            "int"     -> return "Int"
                            "Number"  -> do  case i of
                                                 Just (o, e) -> do{ if hasFloat e
                                                                        then return "Float"
                                                                        else return "Int"
                                                                  } 
                                                 Nothing -> do ni <- getCLArg NumberToInt
                                                               if ni
                                                                   then return "Int"
                                                                   else return "Float"
                                           
                            "String"  -> return "String"
                            "*"       -> return "Dynamic"
                            "Object"  -> return "Dynamic"
                            "Function"-> return "Dynamic"
                            "Array"   -> return "Array<Dynamic>"
                            "XML"     -> return "XML"
                            "RegExp"  -> return "EReg"
                      ; return $ r ++ showw n
                      }
        AsTypeRest -> return $ "Array<Dynamic>"
        AsTypeUser n -> return $ showb n

hasFloat = everything (||) (False `mkQ` isFloat)

isFloat (TokenDouble x) = True
isFloat _ = False

primaryE x = case x of
                 PEThis x -> do{ return $ showb x}
                 PEIdent x -> do{ return $ showb x}
                 PELit x -> do{ return $ showb x}
                 PEArray x -> do{ r <- arrayLit x; return r}
                 PEObject x -> do{ r <- objectLit x; return r}
                 PEXml x -> do{ return $ "Xml.parse(\""++ showd x ++ "\")" ++ showw x}
                 PEFunc x -> do{ r <- funcE x; return r}
                 PEParens l x r -> do{ v <- listE x; return $ showb l ++ v ++ showd r ++ showw r}

arrayLit (ArrayLitC l x r) = do{ return $ showb l ++ maybe "" elision x ++ showb r }

arrayLit (ArrayLit l x r) = do{ e <- elementList x; return $ showb l ++ e ++ showb r}

elementList (El l e el r) = do{ es <- assignE e; els <- foldrM (\(EAE c p) s -> do{ ps <- assignE p; return $ elision c ++ ps ++ s}) "" el; return $ maybeEl elision l ++ es ++ els ++ maybeEl elision r }

elision (Elision x) = showl x

objectLit (ObjectLit l x r) = do{ p <- maybe (return "") propertyNameAndValueList x; return $ showb l ++ p ++ showb r}

propertyNameAndValueList (PropertyList x) = do
    p <- foldrM (\(p, c, e, s) str -> do{ ex <- assignE e; return $ showb p ++ showb c ++ ex ++ maybe "" showb s ++ str}) "" x
    return p

funcE (FuncE f i s b) = do{ x <- block b; s' <- signature s; return $ showb f ++ s' ++ x}

listE (ListE l) = do{ x <- foldrM (\(e, c) s -> do{es <- assignE e; return $ es ++ maybe "" showb c ++ s} ) "" l; return x}

listENoIn = listE

postFixE x = case x of
                 PFFull p o     -> do{ p' <- fullPostFixE p; o' <- postFixUp o; return $ p' ++ o'}
                 PFShortNew p o -> do{ p' <- shortNewE p; o' <- postFixUp o; return $ p' ++ o'}
    where postFixUp o = return $ maybe "" showb o

fullPostFixE x = case x of
                    FPFPrimary p sb  -> do{ e <- primaryE p; sub <- foldsub sb; return $ e ++ sub}
                    FPFFullNew f sb  -> do{ e <- fullNewE f; sub <- foldsub sb; return $ e ++ sub}
                    FPFSuper s p sb  -> do{ e <- superE s; p' <- propertyOp p; sub <- foldsub sb; return $ e ++ p' ++ sub}
    where foldsub sb = foldrM (\a b -> do{c <- fullPostFixSubE a; return $ c ++ b}) "" sb 

fullPostFixSubE x = case x of
                        FPSProperty p -> propertyOp p >>= return
                        FPSArgs     a -> args a >>= return
                        FPSQuery    q -> queryOp q >>= return

fullNewE (FN k e a) = do{ e' <- fullNewSubE e; a' <- args a; return $ showb k ++ e' ++ a'}

fullNewSubE x = case x of
                    FN _ _ _      -> do{ e <- fullNewE x; return e}
                    FNPrimary e p -> do{ e' <- primaryE e; p' <- foldprop p; return $ e' ++ p'}
                    FNSuper e p   -> do{ e' <- superE e; p' <- foldprop p; return $ e' ++ p'}
    where foldprop p = foldrM (\a b -> do{ a' <- propertyOp a; return $ a' ++ b}) "" p

shortNewE (SN k s) = do{ s' <- shortNewSubE s; return $ showb k ++ s'}

shortNewSubE x = case x of
                     SNSFull e  -> fullNewSubE e >>= return
                     SNSShort e -> shortNewE e >>= return

superE (SuperE k p) = do{ p' <- maybe (return "") args p; return $ showb k ++ p'}

propertyOp x = case x of
                   PropertyOp o n  -> return $ showb o ++ showb n
                   PropertyB l e r -> do{ e' <- listE e; return $ showb l ++ e' ++ showb r }

args (Arguments l e r)  = do{ e' <- maybe (return "") listE e; return $ showb l ++ e' ++ showb r}

queryOp x = case x of
                QueryOpDD o n    -> return $ showb o ++ showb n
                QueryOpD o l e r -> do{ e' <- listE e; return $ showb o ++ showb l ++ e' ++ showb r}

unaryE x = case x of
               UEDelete k p   -> do{ p' <- postFixE p; return $ showb k ++ p'}
               UEVoid k p     -> do{ p' <- postFixE p; return $ showb k ++ p'}
               UETypeof k p   -> do{ p' <- postFixE p; return $ showb k ++ p'}
               UEInc o p      -> do{ p' <- postFixE p; return $ showb o ++ p'}
               UEDec o p      -> do{ p' <- postFixE p; return $ showb o ++ p'}
               UEPlus o p     -> do{ p' <- unaryE p; return $ showb o ++ p'}
               UEMinus o p    -> do{ p' <- unaryE p; return $ showb o ++ p'}
               UEBitNot o p   -> do{ p' <- unaryE p; return $ showb o ++ p'}
               UENot o p      -> do{ p' <- unaryE p; return $ showb o ++ p'}
               UEPrimary p    -> postFixE p >>= return

aritE x = case x of
              AEUnary u  -> unaryE u >>= return
              AEBinary _ _ _ -> binaryE x >>= return 

aritENoIn = aritE

binaryE (AEBinary o x y)  
	| showd o == "as" = do{ x' <- aritE x >>= (\c -> return $ splitLR c); y' <- aritE y >>= (\c -> return $ splitLR c); return $ "cast( "++ (x'!!1) ++", "++ (y'!!1) ++")" ++ (y'!!2) }
	| showd o == "is" = do{ x' <- aritE x >>= (\c -> return $ splitLR c); y' <- aritE y >>= (\c -> return $ splitLR c); return $ "Std.is( "++ (x'!!1) ++", "++ (y'!!1) ++")" ++ (y'!!2) }
    | otherwise       = do{ x' <- aritE x; y' <- aritE y; return $ x' ++ showb o ++ y'}


regE (RegE l x r o) = do{ return $ "~"++ showb l ++ showl x ++ showb r ++ maybeEl showb o}

condE (CondRE r) = do{ e <- regE r; return $ e}
condE (CondE e o) = do{ e' <- aritE e; o' <- maybe (return "") (\(q, e1, c, e2) -> do{ e1' <- assignE e1; e2' <- assignE e2; return $ showb q ++ e1' ++ showb c ++ e2'}) o; return $ e' ++ o'}

condENoIn = condE

nonAssignE (NAssignE e o) = do{ e' <- aritE e; o' <- maybe (return "") (\(q, e1, c, e2) -> do{ e1' <- nonAssignE e1; e2' <- nonAssignE e2; return $ showb q ++ e1' ++ showb c ++ e2'}) o; return $ e' ++ o'}

nonAssignENoIn = nonAssignE

assignE x = case x of
                ALogical p o a  -> do{ p' <- postFixE p; a' <- assignE a; return $ p' ++ showb o ++ a' } 
                ACompound p o a -> do{ p' <- postFixE p; a' <- assignE a; return $ p' ++ showb o ++ a' } 
                AAssign p o a   -> do{ p' <- postFixE p; a' <- assignE a; return $ p' ++ showb o ++ a' } 
                ACond e         -> condE e >>= return

assignENoIn = assignE

typeE = nonAssignE
typeENoIn = nonAssignENoIn

expr (Expr x) = assignE x

forS (ForS k l finit s e s1 e1 r b) = 
    do if isIterator finit e e1
           then do let var = findVar finit 
                       start = findInt finit
                       rop = findROperand e
                   bound <- maybe (return "") (\r -> do{ r' <- aritE r; return r' }) rop
                   fblock <- block b
-- convert to iterator
                   return $ showb k ++ showb l ++ maybeEl showd var ++ " in " ++ maybeEl id start ++ "..." ++ bound ++ showb r ++ fblock
           else do fheader <- maybe (return "") cforInit finit
                   ftest <-  maybe (return "") listE e
                   ftail <- maybe (return "") listE e1
                   fblock <- cforBlock b ftail
                   ws <- wsBlock b
-- convert to while
                   return $ fheader ++ ";" ++ init ws ++ "while " ++ showb l ++ ftest ++ showb r ++ fblock
    where cforInit i = do case i of
                             FIListE l -> listE l >>= return
                             FIVarS v  -> varS v >>= return
          --cforBlock (Block l bs r) tail = do{ bi <-  foldlM (\s b -> do{ x <- blockItem b; return $ s ++ x} ) "" bs 
          cforBlock (Block l bs r) tail = do bi <-  blockItemFold bs
                                             return $ showb l ++ bi ++ "\t" ++ tail ++ ";" ++ init (showw l) ++ showb r
          wsBlock (Block l bs r) = return $ showw l

isIterator fi e e1 = hasInteger fi && hasLessThan e && hasPlusPlus e1

hasInteger = everything (||) (False `mkQ` isInteger)

isInteger (TokenInteger x) = True
isInteger _ = False

hasLessThan = everything (||) (False `mkQ` isLessThan)

isLessThan (TokenOp x) = x == "<"
isLessThan _ = False

hasPlusPlus = everything (||) (False `mkQ` isPlusPlus)

isPlusPlus (TokenOp x) = x == "++"
isPlusPlus _ = False

findVar = everything orElse ((Nothing `mkQ` findVB) `extQ` findVA)
findVB (VarBinding n _ _) = Just n
findVA (PEIdent n) = Just n
findVA _ = Nothing

findInt = everything orElse (Nothing `mkQ` findI)
findI (TokenInteger x) = Just x
findI _ = Nothing

findROperand = everything orElse (Nothing `mkQ` findROp)
findROp (AEBinary op l r) = Just r
findROp _ = Nothing
