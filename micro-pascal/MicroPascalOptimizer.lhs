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

> module MicroPascalOptimizer where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import ExprSyntax
> import ExprOptimizer
> import MicroPascalSyntax

> deadCodeElim (While (Val (VBool False)) _ ) tail = tail


> deadCodeElim (If (Val (VBool True)) thenBlk _ ) tail = concatStmts thenBlk tail
> deadCodeElim (If (Val (VBool False)) _ elseBlk) tail = concatStmts elseBlk tail

> deadCodeElim head tail = Stmt head tail

> $(attLabels [("optimizeP", ''Program),("optimizeD", ''Defs),("optimizeSS", ''Stmts),("optimizeS", ''Stmt)])

> optimizeP_asp  
>   =  syn optimizeP p_Program (do name     <- ter ch_name
>                                  defOp    <- at ch_programDefs optimizeD
>                                  bodOp    <- at ch_programBody optimizeSS
>                                  return (Program name defOp bodOp))

>  .+: syn optimizeD p_EmptyDef (return EmptyDef)
>  .+: syn optimizeD p_Def  (do varName       <- ter ch_varName
>                               varType       <- ter ch_varType
>                               tailDefListOp <- at ch_tailDefList optimizeD
>                               return (Def varName varType tailDefListOp))

>  .+: syn optimizeSS p_EmptyStmt (return EmptyStmt)
>  .+: syn optimizeSS p_Stmt  (do headStmt       <- at ch_headStmt optimizeS
>                                 tailStmtListOp <- at ch_tailStmtList optimizeSS
>                                 return (deadCodeElim headStmt tailStmtListOp))

>  .+: syn optimizeS p_Assign (do varName <- ter ch_assignName
>                                 expOpt  <- at ch_assignExpr optimizeE
>                                 return (Assign varName expOpt))
>  .+: syn optimizeS p_If     (do expOpt <- at ch_ifCond optimizeE
>                                 thenOp <- at ch_ifThen optimizeSS
>                                 elseOp <- at ch_ifElse optimizeSS
>                                 return (If expOpt thenOp elseOp))
>  .+: syn optimizeS p_While  (do expOpt <- at ch_whileCond optimizeE
>                                 doOpt  <- at ch_whileDo optimizeSS
>                                 return (While expOpt doOpt))
>  .+: syn optimizeS p_WriteLn (do expOpt <- at ch_writeLnExpr optimizeE
>                                  return (WriteLn expOpt))
>  .+: syn optimizeS p_ReadLn  (do varName  <- ter ch_readLnName
>                                  return (ReadLn varName))

>  .+: optimizeE_asp




> optimizeProgram e = sem_Program optimizeP_asp e emptyAtt #. optimizeP

Tests

> test1_1 = optimizeProgram (Program "test" 
>                           (Def "x" TInt 
>                           EmptyDef)
>                           (Stmt (If (Var "x") 
>                             ((Stmt (WriteLn (Var "x"))) 
>                             EmptyStmt) 
>                             ((Stmt (ReadLn "x")) 
>                             EmptyStmt)) 
>                           EmptyStmt))
> -- Program {name = "test", programDefs = Def {varName = "x", varType = integer, tailDefList = EmptyDef}, programBody = Stmt {headStmt = If {ifCond = Var {var = "x"}, ifThen = Stmt {headStmt = WriteLn {writeLnExpr = Var {var = "x"}}, tailStmtList = EmptyStmt}, ifElse = Stmt {headStmt = ReadLn {readLnName = "y"}, tailStmtList = EmptyStmt}}, tailStmtList = EmptyStmt}}
> test1_2 = optimizeProgram (Program "test" 
>                           (Def "x" TInt 
>                           EmptyDef)
>                           (Stmt (If (Val $ VBool True) 
>                             (Stmt (Assign "x" (Val $ VInt 0)) 
>                             EmptyStmt) 
>                             (Stmt (ReadLn "x")
>                             EmptyStmt)) 
>                           (Stmt (WriteLn (Var "x")) 
>                           EmptyStmt)))
> -- Program {name = "test", programDefs = Def {varName = "x", varType = integer, tailDefList = EmptyDef}, programBody = Stmt {headStmt = Assign {assignName = "x", assignExpr = Val {val = 0}}, tailStmtList = Stmt {headStmt = WriteLn {writeLnExpr = Var {var = "x"}}, tailStmtList = EmptyStmt}}}
> test1_3 = optimizeProgram (Program "test" 
>                           (Def "x" TInt 
>                           EmptyDef)
>                           (Stmt (If (Val $ VBool False) 
>                             (Stmt (Assign "x" (Val $ VInt 0)) 
>                             EmptyStmt) 
>                             (Stmt (ReadLn "x") 
>                             EmptyStmt))
>                           (Stmt (WriteLn (Var "x"))  
>                           EmptyStmt)))
> -- Program {name = "test", programDefs = Def {varName = "x", varType = integer, tailDefList = EmptyDef}, programBody = Stmt {headStmt = ReadLn {readLnName = "y"}, tailStmtList = Stmt {headStmt = WriteLn {writeLnExpr = Var {var = "x"}}, tailStmtList = EmptyStmt}}}

> test2_1 = optimizeProgram (Program "test" 
>                           (Def "x" TInt 
>                           (Def "y" TBool 
>                           EmptyDef))
>                           (Stmt (While (Var "x") 
>                                 (Stmt (ReadLn "y") 
>                                 EmptyStmt))
>                           EmptyStmt))
> -- Program {name = "test", programDefs = Def {varName = "x", varType = integer, tailDefList = Def {varName = "y", varType = boolean, tailDefList = EmptyDef}}, programBody = Stmt {headStmt = While {whileCond = Var {var = "x"}, whileDo = Stmt {headStmt = ReadLn {readLnName = "y"}, tailStmtList = EmptyStmt}}, tailStmtList = EmptyStmt}}
> test2_2 = optimizeProgram (Program "test" 
>                           (Def "x" TInt 
>                           (Def "y" TBool 
>                           EmptyDef))
>                           (Stmt (While (Val $ VBool False) 
>                                 (Stmt (ReadLn "y") 
>                                 EmptyStmt))
>                           (Stmt (WriteLn (Val $ VInt 5))  
>                           EmptyStmt)))
> -- Program {name = "test", programDefs = Def {varName = "x", varType = integer, tailDefList = Def {varName = "y", varType = boolean, tailDefList = EmptyDef}}, programBody = Stmt {headStmt = WriteLn {writeLnExpr = Val {val = 5}}, tailStmtList = EmptyStmt}}
