module CodeGen(genx86) where

import Syntax
import Parser
import Text.Parsec
import Data.List
import Data.Maybe(fromJust)
import qualified Data.Map as Map

type LabelPos = Integer

--test generation
testGen = putStr . gen . rps

genOut = (writeFile "prog.s") . gen . rps

genx86 = gen . rps

gen :: Either ParseError ((Program (Declaration_List decls), Scopes)) -> String
gen p = case p of
  Left _ -> "Invalid parse, no code to generate\n"
  Right (other,scopes) -> genProg other scopes

--File header
genProg ::  Program declList -> Scopes -> String
genProg (InitProgram decls@(InitDeclList declList)) scopes = "\t.global main\n\
    \\t.extern printf\n\
    \\t.extern scanf\n\
    \\t.data\n\
    \fmt:\n\
    \\t.asciz \"%d\\n\"\n\
    \fmtIn:\n\
    \\t.asciz \"%d\"\n" ++ genDecls decls scopes 1

isGlobalVar :: Declaration kind -> Bool
isGlobalVar (InitVarDecl vd) = True
isGlobalVar (InitFuncDecl fd) = False

--Generate global variables first, then functions
genDecls :: Declaration_List decls -> Scopes -> LabelPos -> String
genDecls (InitDeclList decls)  scopes lblpos = (foldl (++) "" $ map genGlobalVar $
    filter (isGlobalVar) decls) ++
    "\t.text\n" ++
    outFuncStr ++
    inFuncStr ++
    (foldl (++) "" $ map (\d -> genDecl d scopes lblpos) $
    filter (not . isGlobalVar) decls)

genDecl :: Declaration kind -> Scopes -> LabelPos -> String
genDecl (InitVarDecl vd) scopes lblpos = "Not needed\n"
genDecl (InitFuncDecl fd) scopes lblpos = genFunc fd scopes lblpos

genFunc :: (Func_Declaration ts id params cmpst) -> Scopes -> LabelPos -> String
genFunc (InitFunc ts ident@(Identifier id) params cs@(InitCompSt localDecls stList)) scopes lblpos = 
    id ++ ": \n" ++ funcPrologue ++ 
        (case Map.lookup ident scopes of
            Nothing -> "DEBUG: Could not find function in code generation!"
            Just entry@(tbl,isFunc,ts,size) -> "\tsub $" ++ (show $ sum $ map frth4 $ filter (not . snd4) tbl))
        ++ ", %esp\
            \ \t /* Allocate space for local variables */ \n"
        ++ (genStList stList  ident scopes lblpos) ++           
        funcEpilogue      

outFuncStr :: String
outFuncStr = "output:\n" ++ funcPrologue ++
    "\tpush 8(%ebp) \t /* Push value to print */\n\
    \\tpush $fmt \t /* Push pointer to format string */ \n\
    \\tcall printf\n\
    \\tadd $8, %esp\n" ++ funcEpilogue

inFuncStr :: String
inFuncStr = "input:\n" ++ funcPrologue ++
    "\tsub $4,%esp\n\
    \\tlea -4(%ebp),%eax\n\
    \\tpush %eax\n\
    \\tpush $fmtIn\n\
    \\tcall scanf\n\
    \\tadd $8, %esp\n\
    \\tlea -4(%ebp),%eax\n\
    \\tpush (%eax)\n" ++ funcEpilogue

funcPrologue = "\tpush %ebp \t /* Save EBP */ \n\
    \\tmov %esp, %ebp \t /* Save current stack pointer in EBP */ \n"
            
funcEpilogue :: String
funcEpilogue = "\tmov (%esp), %eax \t /* Copy return value to EAX */\n\
    \\tmov %ebp, %esp \t /* Deallocate local variables */ \n\
    \\tpop %ebp \t /* Restore EBP */\n\tret \t /* Exit Function */\n"   
    
genStList :: Statement_List stList -> CurrLocation -> Scopes -> LabelPos -> String    
genStList stList@(InitStList statements) loc scopes lblpos = foldl (++) "" $ map (\st -> genSt st  loc scopes lblpos) statements
genStList (InitStListEmpty _) loc scopes lblpos = "\t /* Empty Statement List */\n"

genSt :: Statement st -> CurrLocation -> Scopes -> LabelPos -> String
genSt (InitStExpr exprSt) loc scopes lblpos = genExprSt exprSt loc scopes lblpos
genSt (InitStComp cs@(InitCompSt localDecls stList)) loc scopes lblpos = genStList stList loc scopes lblpos
genSt (InitStSel selSt) loc scopes lblpos = genSelSt selSt loc scopes lblpos
genSt (InitStIter iterSt) loc scopes lblpos = genIterSt iterSt loc scopes lblpos
genSt (InitStRet retSt) loc scopes lblpos = genRetSt retSt loc scopes lblpos

genSelSt :: Selection_Statement expr st st2 -> CurrLocation -> Scopes -> LabelPos -> String
genSelSt (InitIf expr st _) loc scopes lblpos = (genExpr expr loc scopes (lblpos + 1)) ++ "\tcmp $1,(%esp) \t /* Regular if */ \n" ++ "\tjne " ++ (show lblpos) ++ "f\n" ++
                                                (genSt st loc scopes (lblpos + 1)) ++ (show lblpos) ++ ": \t/* ENDIF Cond not true */\n"
genSelSt (InitIfElse expr st st2) loc scopes lblpos = (genExpr expr loc scopes (lblpos + 2)) ++ "\tcmp $1,(%esp)\n" ++ "\tjne " ++ (show lblpos) ++ "f\t /* IF ELSE */\n"
                                                      ++ (genSt st loc scopes (lblpos + 2)) ++ "\tjmp " ++ (show (lblpos + 1)) ++ "f\t/* Skip false */\n" ++ (show lblpos) ++ ":\n" ++
                                                      (genSt st2 loc scopes (lblpos + 2)) ++ (show (lblpos + 1)) ++ ":\t/* ENDIFELSE */\n"

genIterSt :: Iteration_Statement expr st -> CurrLocation -> Scopes -> LabelPos -> String
genIterSt (InitWhile expr st) loc scopes lblpos = (show lblpos) ++ ":\t/* WHILE BEGIN */\n" ++ (genExpr expr loc scopes (lblpos + 2)) ++
                                                  "\tcmp $1,(%esp)\n" ++ "\tjne " ++ (show (lblpos + 1)) ++ "f\n" ++
                                                  (genSt st loc scopes (lblpos + 2)) ++ "\tjmp " ++ (show lblpos) ++
                                                  "b\n" ++ (show (lblpos + 1)) ++ ":\t /* While not true */\n"
                                                      
genRetSt :: Return_Statement expr -> CurrLocation -> Scopes -> LabelPos -> String
genRetSt (InitRet expr) loc scopes lblpos = (genExpr expr loc scopes lblpos) ++ funcEpilogue
genRetSt (InitRetEmpty _) loc scopes lblpos = funcEpilogue

genExprSt :: Expression_Statement expr ->  CurrLocation -> Scopes -> LabelPos -> String
genExprSt (InitExprStExpr expr)  loc scopes lblpos = genExpr expr  loc scopes lblpos
genExprSt (InitExprStNone _)  loc scopes lblpos = "\t /* Empty Expression Statement */ \n"

genExpr :: Expression a b ->  CurrLocation -> Scopes -> LabelPos -> String
genExpr (InitAssignExpr var expr) loc scopes lblpos = (genExpr expr loc scopes lblpos) ++ (genVar var  loc scopes False lblpos) ++
                                                   "\tpop %eax /* Pop address into EAX */\n\
    \\tpopl (%eax) \t /* Pop value to location held in EAX */\n\
    \\tpush %eax \n"
genExpr (InitSimpleExpr simpleExpr _)  loc scopes lblpos = genSimpleExpr (simpleExpr)  loc scopes lblpos

genSimpleExpr :: Simple_Expression ae op ae0 ->  CurrLocation -> Scopes -> LabelPos -> String
genSimpleExpr (InitSim_A aExpr _ _) loc scopes lblpos = genAddExpr aExpr  loc scopes lblpos
genSimpleExpr (InitSim_AOE aExpr0 relop aExpr1) loc scopes lblpos = (genAddExpr (aExpr0)  loc scopes lblpos) ++ (genAddExpr (aExpr1)  loc scopes lblpos) ++ (genRelop relop)

genAddExpr :: Additive_Expression term0 tail ->  CurrLocation -> Scopes -> LabelPos -> String
genAddExpr (InitAE term0 tail)  loc scopes lblpos = (foldl (++) (genTerm (term0)  loc scopes lblpos) (map (\t -> ((genTerm (snd t)  loc scopes lblpos) ++ genAddOp (fst t))) tail))

genTerm :: Term factor0 tail ->  CurrLocation -> Scopes -> LabelPos -> String
genTerm (InitTerm factor0 tail)  loc scopes lblpos = (foldl (++) (genFactor (factor0)  loc scopes lblpos) (map (\f -> ((genFactor (snd f)  loc scopes lblpos) ++ (genMulOp (fst f)))) tail))

genFactor :: Factor f ->  CurrLocation -> Scopes -> LabelPos -> String
genFactor (InitFact_E expr)  loc scopes lblpos = genExpr expr  loc scopes lblpos
genFactor (InitFact_V var)  loc scopes lblpos = genVar var  loc scopes True lblpos
genFactor(InitFact_C call)  loc scopes lblpos = genCall call loc scopes lblpos
genFactor(InitFact_N num)  loc scopes lblpos = genConstant num

genCall :: Call id arguments -> CurrLocation -> Scopes -> LabelPos -> String 
genCall (InitCall ident@(Identifier c) args) loc scopes lblpos = (case args of
    InitArgsEmpty _ -> "\tcall " ++ c ++ "\n"
    InitArgs (InitArgsList exprs) -> (foldl (++) "" $ map (\expr -> genExpr expr loc scopes lblpos) $ reverse exprs) ++
        "\tcall " ++ c ++ "\n" ++ "\tadd $" ++ (show (4 * (length exprs))) ++ ", %esp \
        \\t /* Restore ESP to state prior to argument-pushing */\n") ++
    (case fromJust (Map.lookup ident scopes) of
        (_,_,Void,_) -> ""
        otherwise -> "\tpush %eax /* Push result of function onto stack */\n")
        
genMulOp :: Mul_Op -> String
genMulOp MUL = "\tpop %eax\n\
    \\timul (%esp),%eax \t /* Multiplication */\n\
    \\tmov %eax,(%esp)\n"
genMulOp DIV = "\tpop %eax \t /* EAX = Divisor */\n\
    \\tmov $0, %edx \t /* Clobber higher 32 bits of dividend */\n\
    \\txchg %eax, (%esp) /* Exchange divisor and dividend locations. */\n\
    \\tidivl (%esp) \t /* Division */\n\
    \\tmov %eax, (%esp) \t /* Move quotient to top of stack */\n"

genAddOp :: Add_Op -> String
genAddOp PLUS = "\tpop %eax\n\
    \\tadd %eax, (%esp) \t /* Addition */\n"
genAddOp MINUS = "\tpop %eax\n\
    \\tsub %eax, (%esp) \t /* Subtraction */\n"

genRelop :: Rel_Op -> String
genRelop op = "\tpop %eax\n\tcmp %eax, (%esp)\n" ++ "\tmovl $0,(%esp)\n" ++ (case op of 
    GT_OP -> "\tsetg"
    EQEQ_OP -> "\tsete"
    NE_OP -> "\tsetne"
    GE_OP -> "\tsetge"
    LT_OP -> "\tsetl"
    LT_EQ -> "\tsetle"
    ) ++ " (%esp)\n"
-- Determine whether to retrieve value of a variable, or its address.
-- IE, var = expr (need address) vs function(var) (need value)
type GetValue = Bool 

genVar :: Var id expr -> CurrLocation -> Scopes -> GetValue -> LabelPos -> String
genVar (InitVar id _)  loc scopes getValue lblpos = (whereIsVar id  loc scopes getValue)
genVar (InitVarArr id expr)  loc scopes getValue lblpos = (genVar (InitVar id ())  loc scopes False lblpos) ++ 
    (genExpr expr loc scopes lblpos) ++
    "\tpop %eax /* Array offset expr value */\n\
    \\timul $4,%eax /* Scale offset to  4-byte boundaries */\n\
    \\tadd (%esp),%eax /* EAX = Offsetted location */\n" ++
    if getValue then "\tmov (%eax),%eax \t /* Dereference value at offsetted location. */\n" ++ "\tmov %eax,(%esp) \t /* Put value on top of stack */\n"
        else "\tmov %eax, (%esp) \t /* Move offsetted location to top of stack. */\n"

--   \txchg (%esp),%eax /* Swap places since we need to subtract to offset address */\n\    

whereIsVar :: Identifier ->  CurrLocation -> Scopes -> GetValue -> String
whereIsVar ident@(Identifier x)  funcName scopes getValue = let currScope = Map.lookup funcName scopes in
    case currScope of
        Nothing -> error "DEBUG: COULD NOT FIND FUNCTION IN CODE GENERATION!!"
        Just entry@(tbl,isFunc,ts,size) -> case findIndex (\symbol@(id, _, _, size) -> id == ident) tbl of
            Nothing -> "\tpushl " ++ (if getValue then
                                       if (size == -1)
                                        then x else ("$" ++ x)
                                     else ("$" ++ x)) ++ 
                "\t /* Pushing " ++ (if getValue then "value" else "address") ++ " of global var " ++ (show x) ++ " */\n"
            Just pos -> "\tlea " ++
              (case tbl !! pos of
                --Calculate the offset of a parameter from the list of symbols for this function.
                (_,True,_,_) -> (show ((4 * (length $ filter (snd4) $ take pos tbl)) + 8))
                --Calculate the offset of a local variable from the list of symbols for this function.
                otherwise -> (show $ negate ((sum $ map frth4 $ filter (not . snd4) $ take (pos) tbl) + 4))) ++ 
              ("(%ebp), %eax \t /* EAX = Location of local/param variable " ++ (show x) ++ "*/\n\
                \\tpushl " ++ (if getValue then (case tbl !! pos of
                    (_,False,True,_) -> "%eax \t/*"
                    otherwise -> "(%eax) \t/*") ++ " Push value " 
                 else (case tbl !! pos of 
                         (_,True,True,_) -> "(%eax) \t/*"; 
                         otherwise -> "%eax \t/*")
                ++ " Push address ") ++
                "of " ++ x ++ " to top of stack */ \n")
                                                                                                                 
--Need to learn lenses. 
--Also, realizing I don't need current location to be embedded in grammar.
genGlobalVar :: Declaration kind -> String
genGlobalVar (InitVarDecl (InitPlainVar ts (Identifier n) _ _)) = n ++ ": \n\
    \\t.long\n"
genGlobalVar (InitVarDecl (InitArrVar ts (Identifier n) (Number num) _)) = n ++ ": \n\
    \\t.fill " ++ (show num) ++ " , " ++ (show 4) ++ "\n"

genConstant :: Number -> String
genConstant (Number x) = "\tpush $" ++ show x ++ "\n"
