-- Parse the tokens generated by Lexer
-- TODO: function declarations, 
--       expressions, 
--       updating Array parameter type,
--       for 
--       while/do
--       if
--       case

module ActionhaXe.Parser(Semi, BlockItem(..), Signature(..), Arg(..), Ast(..), Package(..), parseTokens) where

import ActionhaXe.Lexer
import ActionhaXe.Prim
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Perm

type Semi = Maybe CToken
emptyctok = ([],[])

data BlockItem =  Tok        CToken
                | Block      CToken [BlockItem] CToken
                | ImportDecl CToken CToken Semi  -- import identifier ;
                | ClassDecl  [CToken] CToken CToken (Maybe [CToken]) (Maybe [CToken]) BlockItem -- attributes, class, identifier, maybe extends, maybe implements, body
                | MethodDecl [CToken] CToken (Maybe CToken) CToken Signature BlockItem -- attributes, function, maybe get/set, identifier, Signature, body
                | VarDecl    (Maybe [CToken]) CToken CToken CToken AsType Semi -- maybe attributes, var, identifier, :, datatype, ;
                | Regex      CToken
    deriving (Show)

data Signature =  Signature  CToken [Arg] CToken (Maybe (CToken, AsType)) -- left paren, arguments, right paren, :,  return type
    deriving (Show)

data Arg = Arg CToken CToken AsType (Maybe [CToken]) (Maybe CToken) -- arg name, :, type, maybe default value, maybe comma
         | RestArg [CToken] CToken -- ..., name
    deriving (Show)

data Package = Package CToken (Maybe CToken) BlockItem -- package, maybe name, block
    deriving (Show)

data Ast = Program Package AsState
    deriving (Show)

program :: AsParser Ast
program = do{ x <- package; a <- getState; return $ Program x a}

package = do{ p <- kw "package"; i <- optionMaybe(ident); storePackage i;  b <- block; return $ Package p i b }

block = do{ l <- op "{"; enterScope; x <- inBlock; r <- op "}"; exitScope; return $ Block l x r }

inBlock = try(do{ lookAhead( op "}"); return [] })
      <|> try(do{ b <- block; i <- inBlock; return $ [b] ++ i })
      <|> try(do{ x <- importDecl; i <- inBlock; return $ [x] ++ i})
      <|> try(do{ x <- classDecl; i <- inBlock; return $ [x] ++ i})
      <|> try(do{ x <- methodDecl; i <- inBlock; return $ [x] ++ i})
      <|> try(do{ x <- varDecl; i <- inBlock; return $ [x] ++ i})
      <|> try(do{ x <- reg; i <- inBlock; return $ [(Regex x)] ++ i})
      <|> try(do{ x <- anytok; i <- inBlock; return $ [(Tok x)] ++ i})

importDecl = do{ k <- kw "import"; s <- sident; o <- maybeSemi; return $ ImportDecl k s o}

classDecl = do{ a <- classAttributes; k <- kw "class"; i <- ident; e <- optionMaybe(classExtends); im <- optionMaybe(classImplements); storeClass i; b <- block; return $ ClassDecl a k i e im b}

classAttributes = permute $ list <$?> (emptyctok, (try (kw "public") <|> (kw "internal"))) <|?> (emptyctok, kw "static") <|?> (emptyctok, kw "dynamic")
    where list v s d = filter (\a -> fst a /= []) [v,s,d]

classExtends = do{ k <- kw "extends"; s <- nident; return $ k:[s]}

classImplements = do{ k <- kw "implements"; s <- sepByCI1 nident (op ","); return $ k:s}

methodDecl = do{ attr <- methodAttributes; k <- kw "function"; acc <- optionMaybe( try(kw "get") <|> (kw "set")); n <- nident; enterScope; sig <- signature; b <- block; exitScope; storeMethod n; return $ MethodDecl attr k acc n sig b}

methodAttributes = permute $ list <$?> (emptyctok, (try (kw "public") <|> try (kw "private") <|> (kw "protected"))) <|?> (emptyctok, ident) <|?> (emptyctok, kw "override") <|?> (emptyctok, kw "static") <|?> (emptyctok, kw "final") <|?> (emptyctok, kw "native")
    where list v o s f n ns = filter (\a -> fst a /= []) [v,ns,o,s,f,n]

signature = do{ lp <- op "("; a <- sigargs; rp <- op ")"; ret <- optionMaybe ( do{ o <- op ":"; r <- datatype; return (o, r)}); return $ Signature lp a rp ret} -- missing return type means constructor

sigargs = do{ s <- many sigarg; return s}
sigarg = try(do{ a <- ident; o <- op ":"; t <- datatype; d <- optionMaybe( do{ o' <- op "="; a <- defval; return $ [o']++a}); c <- optionMaybe(op ","); storeVar a t; return $ Arg a o t d c})
     <|> do{ d <- count 3 (op "."); i <- ident; storeVar i AsTypeRest; return $ RestArg d i }

defval = do{ x <- manyTill defval' (try (lookAhead (op ",")) <|> lookAhead(op ")")); return x }

defval' = try( do{ x <- kw "null"; return x})
      <|> try( do{ x <- kw "true"; return x})
      <|> try( do{ x <- kw "false"; return x})
      <|> try( do{ x <- ident; return x})
      <|> try( do{ x <- str; return x})
      <|> do{ x <- num; return x}

varDecl = do{ ns <- optionMaybe(varAttributes); k <- kw "var"; n <- nident; c <- op ":"; dt <- datatype; s <- maybeSemi; storeVar n dt; return $ VarDecl ns k n c dt s}
varAttributes = permute $ list <$?> (emptyctok, (try (kw "public") <|> try (kw "private") <|> (kw "protected"))) <|?> (emptyctok, ident) <|?> (emptyctok, kw "static") <|?> (emptyctok, kw "native")
    where list v ns s n = filter (\a -> fst a /= []) [v,ns,s,n]

datatype = try(do{ t <- kw "void";      return $ AsType t})
       <|> try(do{ t <- mid "int";      return $ AsType t})
       <|> try(do{ t <- mid "uint";     return $ AsType t})
       <|> try(do{ t <- mid "Number";   return $ AsType t})
       <|> try(do{ t <- mid "Boolean";  return $ AsType t})
       <|> try(do{ t <- mid "String";   return $ AsType t})
       <|> try(do{ t <- mid "Object";   return $ AsType t})
       <|> try(do{ t <- op "*";         return $ AsType t})
       <|> try(do{ t <- mid "Array";    return $ AsType t})
       <|> try(do{ t <- mid "Function"; return $ AsType t})
       <|> try(do{ t <- mid "RegExp";   return $ AsType t})
       <|> try(do{ t <- mid "XML";      return $ AsType t})
       <|> do{ i <- ident; return $ AsTypeUser i}

parseTokens :: String -> [Token] -> Either ParseError Ast
parseTokens filename ts = runParser program initState filename ts
