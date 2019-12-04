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
> import MicroPascalSyntax

> data Error = Duplicated Ident
>            | Undefined  Ident

> instance Show Error where
>  show (Duplicated      n)  = "Duplicated definition: " ++ n
>  show (Undefined       n)  = "Undefined: " ++ n

> type Errors = [Error]
> type DeclaredVars = [String] 

> $(attLabels [("checkNames", ''Errors), ("declaredVars", ''DeclaredVars) ])

> declaredVars_syn_asp 
>   =  syn declaredVars p_Program (at ch_programDefs declaredVars)
>  .+: syn declaredVars p_EmptyDef (return [])
>  .+: syn declaredVars p_Def (do varName <- ter ch_varName
>                                 defVars  <- at ch_tailDefList declaredVars
>                                 return (varName:defVars))
>  .+: emptyAspect


> declaredVars_inh_asp 
>   =  inh declaredVars p_Program ch_programDefs (at lhs declaredVars)  
>  .+: inh declaredVars p_Program ch_programBody (at ch_programDefs declaredVars) 

1 - se podria mejorar para no tener esto y checkear todo haciendo uso del syn declaredVars en syn checkNames p_Def 

>  .+: inh declaredVars p_Def ch_tailDefList (do decVars <- at lhs declaredVars
>                                                varName <- ter ch_varName
>                                                return (varName:decVars))

>  .+: inh declaredVars p_Stmt ch_headStmt     (at lhs declaredVars) 
>  .+: inh declaredVars p_Stmt ch_tailStmtList (at lhs declaredVars) 

>  .+: inh declaredVars p_Assign ch_assignExpr (at lhs declaredVars)

>  .+: inh declaredVars p_If ch_ifCond (at lhs declaredVars)
>  .+: inh declaredVars p_If ch_ifThen (at lhs declaredVars)
>  .+: inh declaredVars p_If ch_ifElse (at lhs declaredVars)

>  .+: inh declaredVars p_While ch_whileCond (at lhs declaredVars)
>  .+: inh declaredVars p_While ch_whileDo   (at lhs declaredVars)

>  .+: inh declaredVars p_WriteLn ch_writeLnExpr (at lhs declaredVars)

>  .+: inh declaredVars p_Bop ch_leftBop (at lhs declaredVars)  
>  .+: inh declaredVars p_Bop ch_rightBop (at lhs declaredVars)
>  .+: inh declaredVars p_Uop ch_expr (at lhs declaredVars)
>  .+: declaredVars_syn_asp

> checkNames_asp  
>   =  syn checkNames p_Program (do errors  <- at ch_programDefs checkNames
>                                   errors' <- at ch_programBody checkNames
>                                   return (errors ++ errors'))

>  .+: syn checkNames p_EmptyDef (return [])
>  .+: syn checkNames p_Def  (do decVars <- at lhs declaredVars
>                                varName <- ter ch_varName
>                                errors  <- at ch_tailDefList checkNames
>                                return ((if elem varName decVars then [Duplicated varName] else []) ++ errors))

>  .+: syn checkNames p_EmptyStmt (return [])
>  .+: syn checkNames p_Stmt  (do errors  <- at ch_headStmt checkNames
>                                 errors' <- at ch_tailStmtList checkNames
>                                 return (errors ++ errors'))

>  .+: syn checkNames p_Assign (do decVars <- at lhs declaredVars
>                                  varName <- ter ch_assignName
>                                  errors  <- at ch_assignExpr checkNames
>                                  return ((if notElem varName decVars then [Undefined varName] else []) ++ errors))
>  .+: syn checkNames p_If     (do errors   <- at ch_ifCond checkNames
>                                  errors'  <- at ch_ifThen checkNames
>                                  errors'' <- at ch_ifElse checkNames
>                                  return (errors ++ errors' ++ errors''))
>  .+: syn checkNames p_While  (do errors   <- at ch_whileCond checkNames
>                                  errors'  <- at ch_whileDo checkNames
>                                  return (errors ++ errors'))
>  .+: syn checkNames p_WriteLn (at ch_writeLnExpr checkNames)
>  .+: syn checkNames p_ReadLn  (do decVars <- at lhs declaredVars
>                                   varName <- ter ch_readLnName
>                                   return (if notElem varName decVars then [Undefined varName] else []))

>  .+: syn checkNames p_Var (do decVars <- at lhs declaredVars
>                               varName <- ter ch_var
>                               return (if notElem varName decVars then [Undefined varName] else []))
>  .+: syn checkNames p_Val (return [])                   
>  .+: syn checkNames p_Bop (do errors  <- at ch_leftBop checkNames
>                               errors' <- at ch_rightBop checkNames
>                               return (errors ++ errors'))
>  .+: syn checkNames p_Uop (at ch_expr checkNames)
>  .+: declaredVars_inh_asp


> checkProgramNames e = sem_Program checkNames_asp e (declaredVars =. [] *. emptyAtt) #. checkNames

Tests

> ok1 = checkProgramNames (Program "test" EmptyDef (Stmt (WriteLn (Val (VInt 5)) ) EmptyStmt ) )
> ok2 = checkProgramNames (Program "test" (Def "x" TBool EmptyDef ) (Stmt (WriteLn (Var "x" ) ) EmptyStmt ) )
> ok3 = checkProgramNames (Program "test" (Def "x" TBool (Def "y" TBool EmptyDef ) ) (Stmt (WriteLn (Var "x" ) ) EmptyStmt ) )
> fail1 = checkProgramNames (Program "test" EmptyDef (Stmt (WriteLn (Var "x" )) EmptyStmt ) )
> -- [Undefined: x]
> fail2 = checkProgramNames (Program "test" (Def "x" TBool (Def "x" TBool EmptyDef ) ) (Stmt (WriteLn (Var "x" ) ) EmptyStmt ) )
> -- [Duplicated definition: x]
> fail3 = checkProgramNames (Program "test" (Def "x" TBool (Def "x" TBool EmptyDef ) ) (Stmt (WriteLn (Var "y" ) ) EmptyStmt ) )
> -- [Duplicated definition: x,Undefined: y]