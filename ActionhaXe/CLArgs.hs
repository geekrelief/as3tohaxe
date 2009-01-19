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
module ActionhaXe.CLArgs (CLArg(..), Conf(..), clargs) where 

import System.Console.ParseArgs
import Data.Generics
import Data.Map

data CLArg = NumberToInt
           | NoCarriage
           | CreateImports
           | Input
           | OutputDir
    deriving (Eq, Ord, Show, Data, Typeable)

data Conf = Conf{ confArgs::Args CLArg, confInput::String, confOutput::String, imports::Map String [String] }
          | ConfNone
    deriving (Show, Data, Typeable)

initArg f desc = Arg{ argIndex = f, argAbbr = Nothing, argName = Nothing, argData = Nothing, argDesc = desc }

clargs = 
  [
    (initArg NumberToInt "translate :Number to :Int (default :Float)"){ argAbbr = Just 'i', argName = Just "intnum" }
  , (initArg NoCarriage "remove carriage returns '\\r' in output"){ argAbbr = Just 'r', argName = Just "nocarriage" }
  , (initArg CreateImports "creates \"Import\" files in place of * imports (e.g. import a.*;)"){ argAbbr= Just 'c', argName = Just "imports" }
  , (initArg Input "input to convert") { argData = argDataRequired "directory | file" ArgtypeString }
  ]
