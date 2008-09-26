-- Translate an Actionscript 3 AST to haXe and hxml for Flash 9

module ActionhaXe.Translator where

import ActionhaXe.Lexer
import ActionhaXe.Prim
import ActionhaXe.Parser
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Data.Foldable (foldrM)


-- flags
mainPackage = "mainPackage"
fpackage  = "packageName"
fclass = "className"
fclassAttr = "classAttr"

updateFlag flag val = do st <- get
                         put st{flags = Map.insert flag val (flags st)}
                         return ()

deleteFlag flag = do st <- get
                     put st{flags = Map.delete flag (flags st)}
                     return ()

getFlag :: String -> StateT AsState IO String
getFlag flag = do st <- get
                  mval <- Map.lookup flag $ flags st
                  return mval

--translateAs3Ast :: Package -> StateT AsState IO String
translateAs3Ast p = do str <- program p
                       return str

maybeEl f i = maybe "" (\m -> f m) i

--program :: Package -> StateT AsState IO String
program (Package p n b) = do case n of
                                 Just ntok -> do{ updateFlag fpackage $ showd ntok ; x <- packageBlock b; return $ showd p ++" "++ showd ntok ++ ";" ++ showw ntok ++ x}
                                 Nothing   -> do{ updateFlag fpackage mainPackage; x <-packageBlock b; return $ showw p ++ x}

packageBlock (Block l bs r)  = do bi <- foldrM (\b s -> do{ x <- blockItem b; return $ x ++ s} ) "" bs 
                                  return $ showw l ++ bi

block (Block l bs r)  = do bi <-  foldrM (\b s -> do{ x <- blockItem b; return $ x ++ s} ) "" bs 
                           return $ showb l ++ bi ++ showb r

blockItem b = do x <- case b of  -- Use the list monad here to try all possible paths?
                        Tok t                   -> do{ x <- tok t; return x}
                        Block _ _ _             -> do{ x <- block b; return x} 
                        ImportDecl _ _ _        -> do{ return $ importDecl b }
                        ClassDecl _ _ _ _ _ _   -> do{ x <- classDecl b; return x}
                        MethodDecl _ _ _ _ _ _  -> do{ x <- methodDecl b; return x} 
                        VarDecl _ _ _ _ _ _     -> do{ return $ varDecl b}
                        Regex x                 -> do{ return $ "~" ++ showb x}  -- this should be a part of an expression
                 return x

tok t = do let x = showb t
           f <- getFlag fpackage
           return x

importDecl (ImportDecl i n s) = foldr (\t s -> showb t ++ s) "" [i,n] ++ maybeEl showb s  -- look up and adjust

classDecl (ClassDecl a c n e i b) = 
    do updateFlag fclass $ showd n
       updateFlag fclassAttr $ publicAttr a
       inMain <- getFlag fpackage
       if inMain == mainPackage
            then do x <- block b
                    return $ attr a ++ showb c ++ showb n ++ maybeEl showl e ++ implements i ++ x 
            else do x <- block b
                    return $ attr a ++ showb c ++ showb n ++ maybeEl showl e ++ implements i ++ x 
    where publicAttr as = if "public" `elem` map (\a -> showd a) as then "public" else "private"
          attr as = concat $ map (\attr -> case (showd attr) of { "internal" -> "private" ++ showw attr; "public" -> "" ++ showw attr; x -> showb attr }) as
          implements is = maybeEl showl is

methodDecl (MethodDecl a f ac n s b) = 
    do inMain <- getFlag fpackage
       className <- getFlag fclass
       classAttr <- getFlag fclassAttr
       x <- block b
       if inMain == mainPackage && className == (showd n) && classAttr == "public"
           then do return $ "static " ++ showb f ++ "main() "++ x
           else if className == (showd n)
                    then do return $ attr a ++ showb f ++ "new"++showw n ++ signature s ++ x
                    else do return $ attr a ++ showb f ++ accessor ac ++ showb n ++ signature s ++ x
    where attr as = concat $ map (\attr -> case (showd attr) of { "internal" -> "private" ++ showw attr; "protected" -> "public" ++ showw attr; x -> showb attr }) as
          inMainAttr as = concat $ map (\attr -> case (showd attr) of { "internal" -> "private" ++ showw attr; "protected" -> "public" ++ showw attr; x -> showb attr }) as
          accessor ac = maybeEl showb ac
          funcname n = showb n -- if method has same name as class then replace with new
          signature (Signature l args r ret) = showb l ++ showb r ++ rettype ret
              where rettype r = case r of
                                    Just (c, t) -> showb c ++ datatype t
                                    Nothing     -> ""

varDecl (VarDecl ns v n c d s) = namespace ns ++ showl [v,n,c] ++ datatype d ++ maybeEl showb s

namespace ns = case ns of 
                   Just x -> concat $ map (\n -> (case (showd n) of { "protected" -> "public"; _ -> showd n})  ++ showw n) x
                   Nothing -> ""

datatype d = case d of
                 AsType n -> (case (showd n) of
                                  "void"    -> "Void"
                                  "Boolean" -> "Bool"
                                  "uint"    -> "UInt"
                                  "int"     -> "Int"
                                  "Number"  -> "Float"
                                  "String"  -> "String"
                                  "*"       -> "Dynamic"
                                  "Object"  -> "Dynamic"
                                  "Function"-> "Dynamic"
                                  "Array"   -> "Array<Dynamic>"
                                  "XML"     -> "Xml"
                                  "RegExp"  -> "EReg"
                             ) ++ showw n
                 AsTypeRest -> "Array<Dynamic>"
                 AsTypeUser n -> showb n

