-- Translate an Actionscript 3 AST to haXe and hxml for Flash 9

module ActionhaXe.Translator where

import ActionhaXe.Lexer
import ActionhaXe.Prim
import ActionhaXe.Parser
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State


--updateFlag :: (String, Bool) -> StateT AsState IO ()
updateFlag (flag, val) = do st <- get
                            put st{flags = Map.insert flag val (flags st)}
                            return ()

--translateAs3Ast :: Package -> StateT AsState IO String
translateAs3Ast p = do str <- program p
                       return str

--program :: Package -> StateT AsState IO String
program (Package p n b) = do case n of
                                 Just ntok -> do{ updateFlag ("main", False); return $ showd p ++" "++ showd ntok ++ ";" ++ showw ntok ++ packageBlock b}
                                 Nothing   -> do{ updateFlag ("main", True); return $ showw p ++ packageBlock b}

packageBlock (Block l bs r)  = showw l ++ foldr (\b s -> blockItem b ++ s ) "" bs

block (Block l bs r)  = showb l ++ foldr (\b s -> blockItem b ++ s ) "" bs ++ showb r

blockItem b = case b of  -- Use the list monad here to try all possible paths?
                  Tok t                   -> tok b
                  Block _ _ _             -> block b
                  ImportDecl _ _ _        -> importDecl b
                  ClassDecl _ _ _ _ _ _   -> classDecl b
                  MethodDecl _ _ _ _ _ _  -> methodDecl b
                  VarDecl _ _ _ _ _ _      -> varDecl b

tok t = "tok\n"

importDecl (ImportDecl i n s) = foldr (\t s -> showb t ++ s) "" [i,n] ++ maybe "" (\a -> showb a) s  -- look up and adjust

classDecl (ClassDecl a c n e i b) =  attr a ++ showb c ++ showb n ++ maybe "" (\m -> showl m) e ++ maybe "" (\m -> showl m) i ++ block b
    where attr as = concat $ map (\attr -> case (showd attr) of { "internal" -> "private" ++ showw attr; _ -> "" }) as

methodDecl (MethodDecl a f ac n s b) = "method\n"

varDecl (VarDecl ns v n c d s) = "var\n"
