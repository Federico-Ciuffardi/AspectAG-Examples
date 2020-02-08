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

> module MachineLangAltGenerator where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import MicroPascalSyntax
> import MachineLangSyntax
> import MachineLangAltSyntax

> type InstrsArr = [Instr]
> -- $(attLabels [("generate", ''InstrsAlt), ("prevInstrs",''InstrsArr)])
> $(attLabels [("generate", ''InstrsAlt), ("prevInstrs",''InstrsArr)])

> instr2instrAlt instr nxtInstrsAlt prvInstrs = 
>   (case getJump instr of 
>      Nothing -> InstrAlt instr nxtInstrsAlt
>      Just i  -> if i >= 0 then
>                   JumpInstrAlt instr (skip i nxtInstrsAlt) nxtInstrsAlt
>                 else
>                   let recursion = (instr2instrAlt instr nxtInstrsAlt prvInstrs) in
>                   JumpInstrAlt instr (instr2instrAlt_rewind (i+1) (head prvInstrs) recursion (tail prvInstrs))      
>                                      nxtInstrsAlt)

> instr2instrAlt_rewind 0 instr nxtInstrsAlt prvInstrs = instr2instrAlt instr nxtInstrsAlt prvInstrs
> instr2instrAlt_rewind i instr nxtInstrsAlt prvInstrs = instr2instrAlt_rewind (i+1) (head prvInstrs) (instr2instrAlt instr nxtInstrsAlt prvInstrs) (tail prvInstrs) 


> prevInstrs_asp  
>   =  (inh prevInstrs p_Instr ch_tailInstrList $
>        do headInstr  <- ter ch_headInstr
>           tailInstrs <- at lhs prevInstrs
>           return $ headInstr : tailInstrs )
>  .+: emptyAspect

> generate_asp
>   =  (syn generate p_EmptyInstr $ return EmptyInstrAlt)
>  .+: (syn generate p_Instr $
>        do currentnInstr <- ter ch_headInstr
>           prvInstrs <- at lhs prevInstrs
>           nxtInstrsAlt     <- at ch_tailInstrList generate
>           return $ instr2instrAlt currentnInstr nxtInstrsAlt prvInstrs)
>  .+: prevInstrs_asp

> generateAltAux e prvInstrs = pruneJumps $ sem_Instrs generate_asp e (prevInstrs =. prvInstrs *. emptyAtt) #. generate
> generateAlt e = generateAltAux e []

> test0 = generateAlt 
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 1) 
>   $ Instr (PUSH 2) 
>   $ Instr (PUSH 3) 
>   $ EmptyInstr

> test1 = generateAlt 
>   $ Instr (PUSH 9) 
>   $ Instr (PUSH 9) 
>   $ Instr CMP 
>   $ Instr (JMPZ 3) 
>   $ Instr (PUSH 0) 
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 1) 
>   $ EmptyInstr

> test2 = generateAlt 
>   $ Instr (PUSH 0) 
>   $ Instr (STORE "x") 
>   $ Instr (LOAD "x") 
>   $ Instr WRITE 
>   $ Instr (LOAD "x") 
>   $ Instr (PUSH 1) 
>   $ Instr ADD
>   $ Instr (STORE "x") 
>   $ Instr READ
>   $ Instr (JMPZ 2) 
>   $ Instr (JUMP (negate 8))
>   $ EmptyInstr