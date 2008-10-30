module ActionhaXe.CLArgs (CLArg(..), Conf(..), clargs) where 

import System.Console.ParseArgs
import Data.Generics

data CLArg = NumberToInt
           | Input
           | OutputDir
    deriving (Eq, Ord, Show, Data, Typeable)

data Conf = Conf{ confArgs::Args CLArg, confInput::String, confOutput::String }
          | ConfNone
    deriving (Show, Data, Typeable)

initArg f desc = Arg{ argIndex = f, argAbbr = Nothing, argName = Nothing, argData = Nothing, argDesc = desc }

clargs = 
  [
    (initArg NumberToInt "translate :Number to :Int (default :Float)"){ argAbbr = Just 'i', argName = Just "intnum" }
  , (initArg Input "input to convert") { argData = argDataRequired "directory | file" ArgtypeString }
  ]
