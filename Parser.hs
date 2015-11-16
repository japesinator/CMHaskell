{-# LANGUAGE GADTs #-}

module Parser where

import Syntax
import Text.Parsec
import Text.Parsec.Language
import Data.Functor.Identity(Identity)
import qualified Text.Parsec.Token as P
import qualified Data.Map as Map
import Data.Maybe(fromJust)

style = emptyDef {
  P.commentStart = "/*",
  P.commentEnd =  "*/",
  P.commentLine = "",
  P.nestedComments = True,
  P.identStart = letter,
  P.identLetter = alphaNum <|> oneOf "_",
  P.reservedNames = ["if","else","while","return","int","void"],
  P.reservedOpNames = ["==","!=","<",">","<=",">=","+","-","*","/","="],
  P.caseSensitive = False
  }
  
lexer = P.makeTokenParser style
parens = P.parens lexer
ws = P.whiteSpace lexer
brackets = P.brackets lexer
braces = P.braces lexer
reserved = P.reserved lexer
identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
integer = P.integer lexer
comma = P.comma lexer
semi = P.semi lexer
whiteSpace = P.whiteSpace lexer
commaSep1 = P.commaSep1 lexer

parseTS = choice [reserved "int" >> return Int,
                  reserved "void" >> return Void]
parseID = identifier >>= (\x -> return $ Identifier x)
parseNum = integer >>= (\x -> return $ Number x)
parseAddOp = choice [reservedOp "+" >> return PLUS,
                     reservedOp "-" >> return MINUS]
parseMulOp = choice [reservedOp "*" >> return MUL,
                     reservedOp "/" >> return DIV]
parseRelOp = choice [reservedOp "<=" >> return LT_EQ,
                     reservedOp "<" >> return LT_OP,
                     reservedOp ">=" >> return GE_OP,
                     reservedOp ">" >> return GT_OP,
                     reservedOp "==" >> return EQEQ_OP,
                     reservedOp "=" >> return EQ_OP,
                     reservedOp "!=" >> return NE_OP]

type IsArray = Bool
type IsParam = Bool
type IsFunction = Bool
type ReturnType = Type_Specifier
type Size = Integer

type SymEntry = (Identifier, IsParam, IsArray, Size)
type Scopes = Map.Map Identifier ([SymEntry],IsFunction,Type_Specifier,Size)
type CurrLocation = Identifier

type Parser = ParsecT String (Scopes,CurrLocation) (Identity)

global = "!GLOBAL"

parseProgram :: Parser (Program (Declaration_List decls))
parseProgram = try $ do
  whiteSpace
  decls <- parseDeclarationList 
  eof
  return $ InitProgram decls

parseDeclarationList :: Parser (Declaration_List decls)
parseDeclarationList = try $ do
  decls <- many1 parseDeclaration
  return $ InitDeclList decls

parseDeclaration :: Parser (Declaration kind)
parseDeclaration = try $ choice 
    [try parseVarDeclaration >>= (\x -> return $ InitVarDecl x),
    try parseFuncDeclaration >>= (\x -> return $ InitFuncDecl x)]

parseVarDeclaration :: Parser (Var_Declaration ts id num location)
parseVarDeclaration =
  try $ do
      state <- getState
      choice [
        (try $ do
             reserved "int"
             id <- parseID
             num <- brackets parseNum
             semi
             modifyState (\s -> (
                if inGlobal s then
                    if Map.member id $ fst s then error "Global variable redefinition"
                    else ((Map.insert id ([],False,Int,fromNum num) (fst s)),snd s)
                else 
                    case Map.lookup (snd s) (fst s) of
                        Just scope@(tbl, isFunc, ts, size) -> if not $ alreadyDefined id (tbl) then 
                            ((Map.insert (snd s) ((tbl) ++ [(id,False, True ,4 * fromNum num)], isFunc, ts, size) (fst s)),snd s)
                            else error "Local variable redefinition"
                        otherwise -> error "Wtf?!"))
             return $ InitArrVar Int id num (case (snd state) of
                    Identifier "!GLOBAL" -> Global
                    Identifier x -> InFunction)),
        (try $ do
             reserved "int"
             id <- parseID
             semi
             modifyState (\s -> (
                if inGlobal s then
                    if Map.member id $ fst s then error "Global variable redefinition"
                    else ((Map.insert id ([],False,Int,-1) (fst s)),snd s) -- -1 to mark non-array global
                else 
                    case Map.lookup (snd s) (fst s) of
                        Just scope@(tbl, isFunc, ts, size) -> if not $ alreadyDefined id (tbl) then 
                            ((Map.insert (snd s) ((tbl) ++ [(id,False, False,4)], isFunc, ts, size) (fst s)),snd s)
                            else error "Local variable redefinition"
                        otherwise -> error "Wtf?!"))
             return $ InitPlainVar Int id () (case (snd state) of
                    Identifier "!GLOBAL" -> Global
                    Identifier x -> InFunction))
          ] 

alreadyDefined :: Identifier -> [SymEntry] -> Bool
alreadyDefined symbol table = if (length $ filter (\entry -> fst4 entry == symbol) table) > 0 then True else False


fst4 (a,b,c,d) = a
snd4 (a,b,c,d) = b
thrd4 (a,b,c,d) = c
frth4 (a,b,c,d) = d


fromNum (Number n) = n
          
inGlobal st = case snd st of
    Identifier "!GLOBAL" -> True
    otherwise -> False
                    
parseFuncDeclaration :: Parser (Func_Declaration ts id params cmpst)
parseFuncDeclaration = try $ do
  ts <- parseTS
  ident <- parseID
  savedState <- getState
  -- Function already exists in symbol table.
  if (isInTbl ident (fst savedState)) then (fail $ "Function already defined: " ++ 
    show ident) else string ""
  --Otherwise, add function to symbol table.
  modifyState (\s -> ((Map.insert ident ([],True,ts,4) (fst s)), snd s))
  -- Update location 
  modifyState (\s -> (fst s, ident))
  params <- parens parseParams
  --Remember number of arguments
  case params of
    InitParamsSome (InitParamsList ps) ->
      modifyState (\s -> ((let quad@(tbl, isFunc, retType, size) = fromJust (Map.lookup ident (fst s)) in
            Map.insert ident ((tbl) ++ (map (\p -> case p of
                                                    (InitParam ts id b) -> (id,True,False,4)
                                                    (InitParamArr ts id b) -> (id,True,True,4)
                                            ) (ps)), True, retType, size) (fst s), snd s))) >> string ""
    otherwise -> string ""
  cmpst <- parseCmpSt
  --Keep new symbol table, revert location. 
  modifyState (\s -> (fst s, snd savedState))
  return $ InitFunc ts ident params cmpst

--Helpers for extracting values from triples
fst3 (a, b, c) = a
snd3 (a, b, c) = b
thrd3 (a, b, c) = c
  
reduceParams :: [Param ts id b] -> [Bool]
reduceParams p = fmap reduceParam p

reduceParam :: (Param ts id b) -> Bool
reduceParam (InitParam ts id b) = b
reduceParam (InitParamArr ts id b) = b

parseParams = choice [(try $ parseParamList >>= (\l -> return $ InitParamsSome l)),
    ((reserved "void") >> (return $ InitParamsVoid Void))]

parseParamList = try $ do
  params <- try $ commaSep1 parseParam
  return $ InitParamsList params

parseParam = choice 
    [(try $ do { ts <- parseTS; id <- parseID; brackets whiteSpace;                           
        return $ InitParamArr ts id True}),
    (try $ do {ts <- parseTS; id <- parseID;
        return $ InitParam ts id False})]

parseCmpSt = try $ do
    braces (do
        localDecls <- (try $ parseLocalDeclarations)
        stList <- (try $ parseStList)
        return $ InitCompSt localDecls stList)

parseLocalDeclarations = choice 
    [try $ (do 
        decls <- try $ many1 parseVarDeclaration
        return $ InitLocalDecls decls),
    (string "" >> (return $ InitLocalDeclsEmpty ()))]
    
parseStList = choice
    [try $ (do
        stList <- try $ many1 parseSt
        return $ InitStList stList),
    try $ (string "") >> (return $ InitStListEmpty ())]

parseSt = choice
    [(try $ parseExprSt >>= (\est -> return $ InitStExpr est)),
    (try $ parseCmpSt >>= (\cmpSt -> return $ InitStComp cmpSt)),
    (try $ parseSelSt >>= (\selSt -> return $ InitStSel selSt)),
    (try $ parseIterSt >>= (\iterSt -> return $ InitStIter iterSt)),
    (try $ parseRetSt >>= (\retSt -> return $ InitStRet retSt))]

parseExprSt = choice
          [(try $ do  
                expr <- parseExpr
                semi
                return $ InitExprStExpr expr),
          semi >>= (\_ -> return $ InitExprStNone ())]

parseSelSt = choice
           [(try $ do
           reserved "if"
           expr <- parens parseExpr
           st <- parseSt
           reserved "else"
           st2 <- parseSt
           return $ InitIfElse expr st st2),
           (try $ do
           reserved "if"
           expr <- parens parseExpr
           st <- parseSt
           return $ InitIf expr st ())]
            
parseIterSt = try $ do
            reserved "while"
            expr <- parens parseExpr
            st <- parseSt
            return $ InitWhile expr st

parseRetSt = choice
           [(try $ do
                 reserved "return"
                 expr <- parseExpr
                 semi
                 return $ InitRet expr),
           (try $ do
                 reserved "return"
                 semi
                 return $ InitRetEmpty ())]

parseExpr = choice
            [(try $ do
                     var <- parseVar
                     reservedOp "="
                     expr <- parseExpr
                     return $ InitAssignExpr var expr),
             (try $ do
                 simpleExpr <- parseSimpleExpr
                 return $ InitSimpleExpr simpleExpr ())]

parseVar = choice
           [(try $ do
                id <- parseID
                expr <- brackets parseExpr
                return $ InitVarArr id expr),
            (try $ do
                id <- parseID
                return $ InitVar id ())]

parseSimpleExpr = choice
    [(try $ do
        addExpr <- parseAdditiveExpr
        relop <- parseRelOp
        addExpr2 <- parseAdditiveExpr
        return $ InitSim_AOE addExpr relop addExpr2),
    (try $ do
        addExpr <- parseAdditiveExpr
        return $ InitSim_A addExpr () ())]

parseAdditiveExpr = try $ do
    fterm <- parseTerm
    tail <- many (try $ do
        addop <- parseAddOp
        term <- parseTerm
        return $ (addop,term))
    return $ InitAE fterm tail

parseTerm = try $ do
    ffactor <- parseFactor
    tail <- many (try $ do
        op <- parseMulOp
        factor <- parseFactor
        return $ (op,factor))
    return $ InitTerm ffactor tail

parseFactor = choice
    [(try $ parens parseExpr >>= (\expr -> return $ InitFact_E expr)),
    (try $ parseCall >>= (\c -> return $ InitFact_C c)),
    (try $ parseVar >>= (\v -> return $ InitFact_V v)),
    (try $ parseNum >>= (\n -> return $ InitFact_N n))]

parseCall :: Parser (Call id arguments)
parseCall = try $ do
  ident <- parseID
  s <- getState
  args <- parens parseArgs  
  if notInTbl ident (fst s) then (error $ "Function not found: " ++ show ident) 
    else string ""
  case args of
    InitArgs (InitArgsList args0) -> let entry@(tbl, isFunc, _, _) = fromJust (Map.lookup ident (fst s)) in
        if not isFunc then error $ "Identifier " ++ (show ident) ++ "is not a function\n"
            else case (length $ filter (snd4) tbl) /= (length args0) of
                True -> error $ "Incorrect Function call arity for " ++ (show ident) ++ "\n" ++ "Expected: " ++ (show (length $ filter (snd4) tbl)) ++ "\n" ++
                  "Actual: " ++ (show (length args0))
                False -> return $ InitCall ident args
    InitArgsEmpty _ -> let entry@(tbl, isFunc, _, _) = fromJust (Map.lookup ident (fst s)) in
        if not isFunc then error $ "Identifier " ++ (show ident) ++ "is not a function\n"
            else case (length $ filter (snd4) tbl) /= 0 of
                True -> error "Incorrect Function call arity Empty\n"
                False -> return $ InitCall ident args

parseArgs = choice
            [try $ (parseArgsList >>= (\a -> return $ InitArgs a)),
             try $ (string "" >> (return $ InitArgsEmpty ()))]

parseArgsList = try $ do
  (commaSep1 parseExpr) >>= (\e -> return $ InitArgsList e)

isInTbl ident tbl = Map.member ident tbl
notInTbl ident tbl = not $ isInTbl ident tbl

progNState = do
  p <- parseProgram
  s <- getState
  return (p,fst s)

startState = (Map.fromList [((Identifier "output"),([(Identifier "x",True,False,4)],True,Void,4)),
    ((Identifier "input"),([(Identifier "x",False,False,4)],True,Int,4))],
    Identifier "!GLOBAL")

--Give ast 
rp = runParser parseProgram startState "" 

--Give ast and user state
rps = runParser progNState startState ""        
        
testP file = do
      text <- readFile file
      result <- return $ runParser parseProgram startState "" text
      return $ result
