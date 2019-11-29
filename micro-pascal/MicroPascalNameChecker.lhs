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

> module MicroPascalPrettyPrint where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import ExprSyntax
> import MicroPascalSyntax

Example: pretty printing expressions

> type DeclaredVars = [String] 

> $(attLabels [("checkNames", ''Bool), ("declaredVars", ''DeclaredVars)])

meter en declared vars las variables en las def si hay 2 con el mismo nombre entonces False
despues todos los vals  dan true
y las vars si no estan en la lista entonces False
todo lo desmas es un and entre sus hijos

> declaredVars_asp 
>   =  inh declaredVars p_Program ch_programDefs (at lhs declaredVars)  
>  .+: inh declaredVars p_Program ch_programBody (at lhs declaredVars) 

>  .+: inh declaredVars p_ConsDef ch_tailDefList (at lhs declaredVars) 

>  .+: inh declaredVars p_Body ch_bodyStmts (at lhs declaredVars) 

>  .+: inh declaredVars p_ConsStmt ch_headStmt     (at lhs declaredVars) 
>  .+: inh declaredVars p_ConsStmt ch_tailStmtList (at lhs declaredVars) 

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
>  .+: emptyAspect

> checkNames_asp  
>   =  syn checkNames p_Program (do ok  <- at ch_programDefs checkNames
>                                   ok' <- at ch_programBody checkNames
>                                   return (ok && ok'))

>  .+: syn checkNames p_EmptyDef (return True)
>  .+: syn checkNames p_ConsDef  (do decVars <- at lhs declaredVars
>                                    varName <- ter ch_varName
>                                    ok      <- at ch_tailDefList checkNames
>                                    return (notElem varName decVars && ok))

>  .+: syn checkNames p_Body (at ch_bodyStmts checkNames)

>  .+: syn checkNames p_EmptyStmt (return True)
>  .+: syn checkNames p_ConsStmt  (do ok  <- at ch_headStmt checkNames
>                                     ok' <- at ch_tailStmtList checkNames
>                                     return (ok && ok'))

>  .+: syn checkNames p_Assign (do decVars <- at lhs declaredVars
>                                  varName <- ter ch_assignName
>                                  ok      <- at ch_assignExpr checkNames
>                                  return (elem varName decVars && ok))
>  .+: syn checkNames p_If     (do ok   <- at ch_ifCond checkNames
>                                  ok'  <- at ch_ifThen checkNames
>                                  ok'' <- at ch_ifElse checkNames
>                                  return (ok && ok' && ok''))
>  .+: syn checkNames p_While  (do ok   <- at ch_whileCond checkNames
>                                  ok'  <- at ch_whileDo checkNames
>                                  return (ok && ok'))
>  .+: syn checkNames p_WriteLn (at ch_writeLnExpr checkNames)
>  .+: syn checkNames p_ReadLn  (do decVars <- at lhs declaredVars
>                                   varName <- ter ch_readLnName
>                                   return (elem varName decVars))

>  .+: syn checkNames p_Var (do decVars <- at lhs declaredVars
>                               varName <- ter ch_var
>                               return (elem varName decVars))
>  .+: syn checkNames p_Val (return True)                   
>  .+: syn checkNames p_Bop (do okL  <- at ch_leftBop checkNames
>                               okR <- at ch_rightBop checkNames
>                               return (okL && okR))
>  .+: syn checkNames p_Uop (do ok <- at ch_expr checkNames
>                               return ok)
>  .+: declaredVars_asp


 checkProgramNames e = sem_Program checkNames_asp e (declaredVars =. [] *. emptyAtt) #. checkNames

 test = checkProgramNames (Bop (Var "x") Or (Uop Not (Val (VBool True))))
