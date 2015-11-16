{-# LANGUAGE StandaloneDeriving, GADTs #-}

module Syntax where

data Location = InFunction | Global deriving (Show)

data Program decls where
  InitProgram :: Declaration_List [decl] -> Program decls

deriving instance Show (Program decls)

data Declaration_List decls where
  InitDeclList :: [Declaration kind] -> Declaration_List decls

deriving instance Show (Declaration_List decls)

data Declaration kind where
  InitVarDecl :: Var_Declaration ts id num location -> Declaration kind --Actually don't need location here
  InitFuncDecl :: Func_Declaration ts id params cmpst -> Declaration kind

deriving instance Show (Declaration kind)

data Var_Declaration ts id num location where
  InitPlainVar :: Type_Specifier -> Identifier -> () -> Location -> Var_Declaration ts id num location
  InitArrVar :: Type_Specifier -> Identifier -> Number -> Location -> Var_Declaration ts id num location

deriving instance Show (Var_Declaration ts id num location)

data Func_Declaration ts id params cmpst where
  InitFunc :: Type_Specifier -> Identifier -> Params [param] -> Compound_Statement ldecls stList ->
              Func_Declaration ts id params cmpst
  
deriving instance Show (Func_Declaration ts id params cmpst)

data Params params where
  InitParamsSome :: Params_List [param] -> Params params
  InitParamsVoid :: Type_Specifier -> Params params --How could you make this only take void?

deriving instance Show (Params params)

data Params_List params where
  InitParamsList :: [Param ts id b] -> Params_List params

deriving instance Show (Params_List params)

data Param ts id b where
  InitParam :: Type_Specifier -> Identifier -> Bool -> Param ts id b
  InitParamArr :: Type_Specifier -> Identifier -> Bool -> Param ts id b

deriving instance Show (Param ts id b)

data Compound_Statement ldecls stList where
  InitCompSt :: Local_Declarations [varDecl] -> Statement_List [st] -> Compound_Statement ldecls stList

deriving instance Show (Compound_Statement ldecls stList)

data Local_Declarations decls where
  InitLocalDecls :: [Var_Declaration ts id num location] -> Local_Declarations decls
  InitLocalDeclsEmpty :: () -> Local_Declarations decls

deriving instance Show (Local_Declarations decls)

data Statement_List sts where
  InitStList :: [Statement st] -> Statement_List sts
  InitStListEmpty :: () -> Statement_List sts

deriving instance Show (Statement_List sts)

data Statement st where
  InitStExpr :: Expression_Statement expr -> Statement st
  InitStComp :: Compound_Statement ldecls stList -> Statement st
  InitStSel :: Selection_Statement expr st0 st1 -> Statement st
  InitStIter :: Iteration_Statement expr st0 -> Statement st
  InitStRet :: Return_Statement expr -> Statement st

deriving instance Show (Statement st)

data Expression_Statement expr where
  InitExprStExpr :: Expression a b -> Expression_Statement expr
  InitExprStNone :: () -> Expression_Statement expr

deriving instance Show (Expression_Statement expr)

data Selection_Statement expr st st2 where
  InitIf :: Expression a b -> Statement st0 -> () -> Selection_Statement expr st st2
  InitIfElse :: Expression a b -> Statement st0 -> Statement st1 -> Selection_Statement expr st st2

deriving instance Show (Selection_Statement expr st st2)

data Iteration_Statement expr st where
  InitWhile :: Expression a b -> Statement st0 -> Iteration_Statement expr st

deriving instance Show (Iteration_Statement expr st)

data Return_Statement expr where
  InitRet :: Expression a b -> Return_Statement expr
  InitRetEmpty :: () -> Return_Statement expr

deriving instance Show (Return_Statement expr)

data Expression a b where
  InitAssignExpr :: Var id expr -> Expression a0 b0 -> Expression a b
  InitSimpleExpr :: Simple_Expression ae0 op ae1 -> () -> Expression a b

deriving instance Show (Expression a b)

data Var id expr where
  InitVar :: Identifier -> () -> Var id expr
  InitVarArr :: Identifier -> Expression a b -> Var id expr

deriving instance Show (Var id expr)

data Simple_Expression ae op ae0 where
  InitSim_A :: Additive_Expression term tail -> () -> () -> Simple_Expression ae op ae0
  InitSim_AOE :: Additive_Expression term tail -> Rel_Op -> Additive_Expression term0 tail0 ->
    Simple_Expression ae op ae0

deriving instance Show (Simple_Expression ae op ae0)

data Additive_Expression term0 tail where
  InitAE :: Term f0 t0 -> [((Add_Op),(Term f1 t1))] -> Additive_Expression term0 tail

deriving instance Show (Additive_Expression term0 tail)

data Term factor tail where
  InitTerm :: Factor f -> [((Mul_Op),(Factor f0))] -> Term factor tail

deriving instance Show (Term factor tail)

data Factor f where
  InitFact_E :: Expression a b -> Factor f
  InitFact_V :: Var id expr -> Factor f
  InitFact_C :: Call id arguments -> Factor f
  InitFact_N :: Number -> Factor f

deriving instance Show (Factor f)

data Call id arguments where
  InitCall :: Identifier -> Args argsList -> Call id arguments

deriving instance Show (Call id arguments)

data Args argsList where
  InitArgs :: Args_List [Expression a b] -> Args argsList
  InitArgsEmpty :: () -> Args argsList

deriving instance Show (Args argsList)

data Args_List argsList where
  InitArgsList :: [Expression a b] -> Args_List argsList

deriving instance Show (Args_List argsList)

data Mul_Op = MUL | DIV deriving (Show)

data Add_Op = PLUS | MINUS deriving (Show)

data Rel_Op = LT_EQ | LT_OP | GT_OP | GE_OP | EQEQ_OP | NE_OP | EQ_OP deriving (Show)

data Type_Specifier = Int | Void deriving (Eq,Show)

data Identifier = Identifier String deriving (Show, Ord, Eq)
data Number = Number Integer deriving (Show)
