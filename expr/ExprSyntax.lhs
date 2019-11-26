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


> module ExprSyntax where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH

Val definition

> data Val = VBool Bool  | VInt Integer
>   deriving (Read)
> instance Eq Val where
>    VInt  i == VInt j   = i == j
>    VInt  i == a        = error (show(a) ++ " is not of type VInt")
>    VBool b == VBool b' = b == b'
>    VBool b == a        = error (show(a) ++ " is not of type VBool")
> instance Show Val where
>    show (VBool b) = show b
>    show (VInt i)  = show i

Expr definition

> $(addNont "Expr")

> $(addProd "Val" ''Nt_Expr
>   [  ("val", Ter ''Val)])

> $(addProd "Var" ''Nt_Expr
>   [  ("var", Ter ''String)])

> data Bop = Or | And | Equ | Lt | Add | Sub | Mul | Div | Mod
>   deriving (Eq, Read, Show)
> $(addProd "Bop" ''Nt_Expr
>   [  ("leftBop" ,  NonTer ''Nt_Expr),
>      ("bop"     ,  Ter ''Bop),
>      ("rightBop",  NonTer ''Nt_Expr)])

> data Uop = Not | Neg
>   deriving (Eq, Read, Show)
> $(addProd "Uop" ''Nt_Expr
>   [  ("uop"     ,  Ter ''Uop),
>      ("expr",  NonTer ''Nt_Expr)])

> $(closeNTs [''Nt_Expr])

> $(mkSemFunc ''Nt_Expr)