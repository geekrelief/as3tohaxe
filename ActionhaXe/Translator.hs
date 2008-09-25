-- Translate an Actionscript 3 AST to haXe and hxml for Flash 9

module ActionhaXe.Translator where

import ActionhaXe.Lexer
import ActionhaXe.Prim
import ActionhaXe.Parser
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State


updateFlag :: (String, Bool) -> State AsState ()
updateFlag (flag, val) = do st <- get
                            put st{flags = Map.insert flag val (flags st)}
                            return ()

translateAs3Ast :: Package -> State AsState String
translateAs3Ast p = do str <- program p
                       return str

program :: Package -> State AsState String
program (Package p n b) = do case n of
                                 Just ntok -> do{ updateFlag ("main", False); return $ showd p ++" "++ showd ntok ++ ";" ++ showw ntok ++ block b}
                                 Nothing   -> return $ shown p ++"indented"

block b = case b of
                 _ -> " block"
