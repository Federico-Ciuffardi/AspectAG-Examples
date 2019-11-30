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
> $(addNont "Stmts")
> $(addNont "Stmt")


> $(addProd "Program" ''Nt_Program [("name", Ter ''Ident),
>                                   ("programDefs", NonTer ''Nt_Defs),
>                                   ("programBody", NonTer ''Nt_Stmts)])

> $(addProd "EmptyDef" ''Nt_Defs [])
> $(addProd "Def" ''Nt_Defs  [("varName", Ter ''Ident),
>                              ("varType", Ter ''Type),
>                              ("tailDefList", NonTer ''Nt_Defs)])


> $(addProd "EmptyStmt" ''Nt_Stmts [] )
> $(addProd "Stmt"  ''Nt_Stmts [("headStmt", NonTer ''Nt_Stmt),
>                                   ("tailStmtList", NonTer ''Nt_Stmts)])

> $(addProd "Assign" ''Nt_Stmt [("assignName", Ter ''Ident),
>                               ("assignExpr", NonTer ''Nt_Expr)])
> $(addProd "If" ''Nt_Stmt [("ifCond", NonTer ''Nt_Expr),
>                           ("ifThen", NonTer ''Nt_Stmts),
>                           ("ifElse", NonTer ''Nt_Stmts)])
> $(addProd "While" ''Nt_Stmt [("whileCond", NonTer ''Nt_Expr),
>                              ("whileDo" , NonTer ''Nt_Stmts)])
> $(addProd "WriteLn" ''Nt_Stmt [("writeLnExpr", NonTer ''Nt_Expr)])
> $(addProd "ReadLn" ''Nt_Stmt [("readLnName", Ter ''Ident)])

> $(closeNTs [''Nt_Program, ''Nt_Stmts, ''Nt_Defs, ''Nt_Stmt])

> $(mkSemFuncs [''Nt_Program, ''Nt_Stmts, ''Nt_Defs, ''Nt_Stmt])
