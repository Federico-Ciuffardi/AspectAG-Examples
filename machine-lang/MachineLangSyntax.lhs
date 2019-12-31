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

> module MachineLangSyntax where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH

> type Shift = Int
> type Var   = String

> data Instr = NEG
>            | ADD
>            | SUB
>            | MUL
>            | DIV
>            | MOD
>            | CMP
>            | PUSH  Integer
>            | JUMP  Shift
>            | JMPZ  Shift
>            | LOAD  Var
>            | STORE Var
>            | READ
>            | WRITE
>            | SKIP
>  deriving (Show,Read,Eq)

> $(addNont "Instrs")

> $(addProd "EmptyInstr" ''Nt_Instrs [] )
> $(addProd "Instr"  ''Nt_Instrs [("headInstr", Ter ''Instr),
>                                 ("tailInstrList", NonTer ''Nt_Instrs)])

> $(closeNTs [''Nt_Instrs])

> $(mkSemFuncs [''Nt_Instrs])

aux funcs

> concatInstrs (Instr head tail)       instrs = Instr head (concatInstrs tail instrs)
> concatInstrs EmptyInstr              instrs = instrs

> instrCount EmptyInstr        = 0
> instrCount (Instr head tail) = 1 + instrCount tail

> arrToInstrs (h:tail) = Instr h (arrToInstrs tail)
> arrToInstrs []       = EmptyInstr

> instrsToArr (Instr h t)  = h : instrsToArr t 
> instrsToArr EmptyInstr = []
