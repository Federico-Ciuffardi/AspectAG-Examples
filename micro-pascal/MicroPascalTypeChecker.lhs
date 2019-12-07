> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE FlexibleContexts  #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeFamilies #-}

> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE DataKinds #-}

> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeApplications #-}
> {-# LANGUAGE EmptyDataDeriving #-}

> module MicroPascalNameChecker where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import ExprSyntax
> import ExprTypeChecker
> import Data.Maybe
> import MicroPascalSyntax

> checkType typeOrErr expected = if isOk typeOrErr then
>                                  if fromOk typeOrErr == expected then
>                                    []
>                                  else
>                                    [Expected expected (fromOk typeOrErr)]
>                                else 
>                                  fromFail typeOrErr


> type Errors = [Error]

> $(attLabels [("checkTypes", ''Errors)])

> declaredVarTypes_syn_asp 
>   =  syn declaredVarTypes p_Program (at ch_programDefs declaredVarTypes)
>  .+: syn declaredVarTypes p_EmptyDef (return [])
>  .+: syn declaredVarTypes p_Def (do varName <- ter ch_varName
>                                     varType <- ter ch_varType
>                                     defVars <- at ch_tailDefList declaredVarTypes
>                                     return ((varName,varType):defVars))
>  .+: exprType_asp


> declaredVarTypes_inh_asp 
>   =  inh declaredVarTypes p_Program ch_programDefs (at lhs declaredVarTypes)  
>  .+: inh declaredVarTypes p_Program ch_programBody (at ch_programDefs declaredVarTypes) 

>  .+: inh declaredVarTypes p_Def ch_tailDefList (return [])

>  .+: inh declaredVarTypes p_Stmt ch_headStmt     (at lhs declaredVarTypes) 
>  .+: inh declaredVarTypes p_Stmt ch_tailStmtList (at lhs declaredVarTypes) 

>  .+: inh declaredVarTypes p_Assign ch_assignExpr (at lhs declaredVarTypes)

>  .+: inh declaredVarTypes p_If ch_ifCond (at lhs declaredVarTypes)
>  .+: inh declaredVarTypes p_If ch_ifThen (at lhs declaredVarTypes)
>  .+: inh declaredVarTypes p_If ch_ifElse (at lhs declaredVarTypes)

>  .+: inh declaredVarTypes p_While ch_whileCond (at lhs declaredVarTypes)
>  .+: inh declaredVarTypes p_While ch_whileDo   (at lhs declaredVarTypes)

>  .+: inh declaredVarTypes p_WriteLn ch_writeLnExpr (at lhs declaredVarTypes)

>  .+: declaredVarTypes_syn_asp

> checkTypes_asp  
>   =  syn checkTypes p_Program (do errors  <- at ch_programDefs checkTypes
>                                   errors' <- at ch_programBody checkTypes
>                                   return (errors ++ errors'))

>  .+: syn checkTypes p_EmptyDef (return [])
>  .+: syn checkTypes p_Def  (return [])

>  .+: syn checkTypes p_EmptyStmt (return [])
>  .+: syn checkTypes p_Stmt  (do errors  <- at ch_headStmt checkTypes
>                                 errors' <- at ch_tailStmtList checkTypes
>                                 return (errors ++ errors'))

>  .+: syn checkTypes p_Assign (do decVarsT  <- at lhs declaredVarTypes
>                                  varName   <- ter ch_assignName
>                                  typeOrErr <- at ch_assignExpr exprType
>                                  return (checkType typeOrErr (fromJust $ lookup varName decVarsT)))
>  .+: syn checkTypes p_If     (do typeOrErr <- at ch_ifCond exprType
>                                  errors'   <- at ch_ifThen checkTypes
>                                  errors''  <- at ch_ifElse checkTypes
>                                  return ((checkType typeOrErr TBool) ++ errors' ++ errors''))
>  .+: syn checkTypes p_While  (do typeOrErr   <- at ch_whileCond exprType
>                                  errors'     <- at ch_whileDo checkTypes
>                                  return ((checkType typeOrErr TBool) ++ errors'))
>  .+: syn checkTypes p_WriteLn (do typeOrErr   <- at ch_writeLnExpr exprType
>                                   return (checkType typeOrErr TInt))
>  .+: syn checkTypes p_ReadLn  (do decVarsT <- at lhs declaredVarTypes
>                                   varName  <- ter ch_readLnName
>                                   return (if (fromJust $ lookup varName decVarsT) == TInt then [] 
>                                          else [Expected TInt (fromJust $ lookup varName decVarsT)]))

>  .+: declaredVarTypes_inh_asp




> checkProgramTypes e = sem_Program checkTypes_asp e (declaredVarTypes =. [] *. emptyAtt) #. checkTypes

Tests

> ok1 = checkProgramTypes (Program "test" (Def "x" TBool EmptyDef) (Stmt (Assign "x" (Bop (Val (VBool True)) And (Var "x" ))) EmptyStmt) )
> -- []
> ok2 = checkProgramTypes (Program "test" (Def "x" TInt EmptyDef) ((Stmt (WriteLn (Var "x")) ) EmptyStmt))
> -- []
> ok3 = checkProgramTypes (Program "test" (Def "x" TInt (Def "y" TBool EmptyDef)) (Stmt (If (Var "y") ((Stmt (WriteLn (Var "x")) ) EmptyStmt) ((Stmt (WriteLn (Var "x")) ) EmptyStmt) ) EmptyStmt) )
> -- [] 
> ok4 = checkProgramTypes (Program "test" (Def "x" TInt (Def "y" TBool EmptyDef)) (Stmt (While (Var "y") (Stmt (ReadLn "x") EmptyStmt) )EmptyStmt) )
> -- [] 
> fail1 = checkProgramTypes (Program "test" (Def "x" TBool EmptyDef) (Stmt (Assign "x" (Bop (Val (VInt 22)) Add (Val (VInt 10)))) EmptyStmt) )
> -- [Expected: boolean Actual: integer]
> fail2 = checkProgramTypes (Program "test" (Def "x" TBool EmptyDef) ((Stmt (WriteLn (Var "x")) ) EmptyStmt))
> -- [Expected: integer Actual: boolean]
> fail3 = checkProgramTypes (Program "test" (Def "x" TInt EmptyDef) (Stmt (If (Var "x") ((Stmt (WriteLn (Var "x")) ) EmptyStmt) ((Stmt (WriteLn (Var "x")) ) EmptyStmt) ) EmptyStmt) )
> -- [Expected: integer Actual: boolean]
> fail4 = checkProgramTypes (Program "test" (Def "x" TInt (Def "y" TBool EmptyDef)) (Stmt (While (Var "x") (Stmt (ReadLn "y") EmptyStmt) )EmptyStmt) )
> -- [Expected: boolean Actual: integer,Expected: integer Actual: boolean]
