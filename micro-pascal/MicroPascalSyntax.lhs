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
> $(addNont "Defs")
> $(addNont "Body")
> $(addNont "Stmts")
> $(addNont "Stmt")


> $(addProd "Program" ''Nt_Program [("programName", Ter ''Ident),
>                                   ("programDefs", NonTer ''Nt_Defs),
>                                   ("programBody", NonTer ''Nt_Body)])

> $(addProd "EmptyDef" ''Nt_Defs [])
> $(addProd "ConsDef" ''Nt_Defs  [("varName", Ter ''Ident),
>                                 ("varType", Ter ''Type),
>                                 ("tailDefList", NonTer ''Nt_Defs)])


> $(addProd "Body" ''Nt_Body [("bodyStmts", NonTer ''Nt_Stmts)])

> $(addProd "EmptyStmt" ''Nt_Stmts [] )
> $(addProd "ConsStmt"  ''Nt_Stmts [("headStmt", NonTer ''Nt_Stmt),
>                                   ("tailStmtList", NonTer ''Nt_Stmts)])

> $(addProd "Assign" ''Nt_Stmt [("assignName", Ter ''Ident),
>                               ("assignExpr", NonTer ''Nt_Expr)])
> $(addProd "If" ''Nt_Stmt [("ifCond", NonTer ''Nt_Expr),
>                           ("ifThen", NonTer ''Nt_Body),
>                           ("ifElse", NonTer ''Nt_Body)])
> $(addProd "While" ''Nt_Stmt [("whileCond", NonTer ''Nt_Expr),
>                              ("whileDo" , NonTer ''Nt_Body)])
> $(addProd "WriteLn" ''Nt_Stmt [("writeLnExpr", NonTer ''Nt_Expr)])
> $(addProd "ReadLn" ''Nt_Stmt [("readLnName", Ter ''Ident)])

> $(closeNTs [''Nt_Program, ''Nt_Body, ''Nt_Stmts,
>             ''Nt_Defs, ''Nt_Stmt])

> $(mkSemFuncs [''Nt_Program, ''Nt_Body, ''Nt_Stmts,
>               ''Nt_Defs, ''Nt_Stmt])
