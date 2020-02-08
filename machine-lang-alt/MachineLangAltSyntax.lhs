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

> module MachineLangAltSyntax where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import MachineLangSyntax

> $(addNont "InstrsAlt")

> $(addProd "EmptyInstrAlt"     ''Nt_InstrsAlt [] )
> $(addProd "InstrAlt"          ''Nt_InstrsAlt [("headInstrAlt", Ter ''Instr),
>                                               ("tailInstrAltList", NonTer ''Nt_InstrsAlt)])
> $(addProd "JumpInstrAlt"     ''Nt_InstrsAlt [("cond", Ter ''Instr),
>                                              ("jTailInstrList" , NonTer ''Nt_InstrsAlt),
>                                              ("njTailInstrList", NonTer ''Nt_InstrsAlt)])


> $(closeNTs [''Nt_InstrsAlt])

> $(mkSemFuncs [''Nt_InstrsAlt])

aux funcs

> getJump (JUMP i) = Just i
> getJump (JMPZ i) = Just i
> getJump _        = Nothing

> skip 1 instAlt = instAlt
> skip i (InstrAlt head tail) = skip (i-1) tail
> skip i (JumpInstrAlt head  _ njTail) = skip (i-1) njTail
> skip i EmptyInstrAlt = EmptyInstrAlt


> pruneJumps (InstrAlt head tail) = InstrAlt head (pruneJumps tail)
> pruneJumps (JumpInstrAlt (JUMP _) jTail njTail) = pruneJumps jTail
> pruneJumps (JumpInstrAlt head jTail njTail) = JumpInstrAlt head (pruneJumps jTail) (pruneJumps njTail)
> pruneJumps EmptyInstrAlt = EmptyInstrAlt




 concatInstrsAlt (InstrAlt head tail)       instrs = InstrAlt head (concatInstrsAlt tail instrs)
 concatInstrsAlt EmptyInstrAlt              instrs = instrs

 instrAltCount EmptyInstrAlt        = 0
 instrAltCount (InstrAlt head tail) = 1 + instrAltCount tail

 arrToInstrsAlt (h:tail) = InstrAlt h (arrToInstrsAlt tail)
 arrToInstrsAlt []       = EmptyInstrAlt

 instrsAltToArr (InstrAlt h t)  = h : instrsAltToArr t 
 instrsAltToArr EmptyInstrAlt = []
