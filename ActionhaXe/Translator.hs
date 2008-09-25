-- Translate an Actionscript 3 AST to haXe and hxml for Flash 9

module ActionhaXe.Translator where

import ActionhaXe.Lexer
import ActionhaXe.Prim
import ActionhaXe.Parser
import Data.Map (Map)
import qualified Data.Map as Map


updateFlag st (flag, val) = st{flags = Map.insert flag val (flags st)}

translateAs3Ast (Program ast st) = ("", program ast st)
                                                                                   
program (Package p n b) st = case n of
                                 Just ntok -> showw p ++ showw ntok ++ block b (updateFlag st ("main", False))
                                 Nothing   -> shown p ++"indented"

block b st = case b of
                 _ -> " block"
