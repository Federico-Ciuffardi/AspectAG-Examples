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

> module MicroPascalSyntax where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import ExprSyntax

> data Type = TyBool | TyInt
>   deriving (Show, Eq, Read)

> type Ident = String

> $(addNont "Program")
> $(addNont "Defs"); $(addNont "DefList")
> $(addNont "Body"); $(addNont "StmtList")
> $(addNont "Stmt")


> $(addProd "Program" ''Nt_Program [("programName", Ter ''Ident),
>                                   ("programDefs", NonTer ''Nt_Defs),
>                                   ("programBody", NonTer ''Nt_Body)])

> $(addProd "Defs" ''Nt_Defs [("defList", NonTer ''Nt_DefList)])
> $(addProd "EmptyDef" ''Nt_DefList [])
> $(addProd "ConsDef" ''Nt_DefList  [("varName", Ter ''Ident),
>                                    ("varType", Ter ''Type),
>                                    ("tailDefList", NonTer ''Nt_Defs)])



> $(addProd "ConsStmt"    ''Nt_StmtList [("headStmt", NonTer ''Nt_Stmt),
>                                        ("tailStmtList", NonTer ''Nt_StmtList)])
> $(addProd "SingleStmt"  ''Nt_StmtList [("stmtLast", NonTer ''Nt_Stmt)] )
> $(addProd "Body" ''Nt_Body [("bodyStmts", NonTer ''Nt_StmtList)])


> $(addProd "Assign" ''Nt_Stmt [("assignName", Ter ''Ident),
>                               ("assignExpr", NonTer ''Nt_Expr)])
> $(addProd "If" ''Nt_Stmt [("ifCond", NonTer ''Nt_Expr),
>                           ("ifThen", NonTer ''Nt_Body),
>                           ("ifElse", NonTer ''Nt_Body)])
> $(addProd "While" ''Nt_Stmt [("whileCond", NonTer ''Nt_Expr),
>                              ("whileDo" , NonTer ''Nt_Body)])
> $(addProd "WriteLn" ''Nt_Stmt [("writeLnExpr", NonTer ''Nt_Expr)])
> $(addProd "ReadLn" ''Nt_Stmt [("readLnName", Ter ''Ident)])

> $(closeNTs [''Nt_Program, ''Nt_Body, ''Nt_StmtList,
>             ''Nt_DefList, ''Nt_Defs, ''Nt_Stmt])

> $(mkSemFuncs [''Nt_Program, ''Nt_Body, ''Nt_StmtList,
>             ''Nt_DefList, ''Nt_Defs, ''Nt_Stmt])