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
                                 Just ntok -> do{ liftIO $ putStrLn "in package"; updateFlag ("main", False); return $ showd p ++" "++ showd ntok ++ ";" ++ showw ntok ++ block b}
                                 Nothing   -> return $ shown p ++"indented"

block b = case b of
                 _ -> " block"
