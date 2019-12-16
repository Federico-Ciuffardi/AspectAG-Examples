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

> module ExprCodeGenerator where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import ExprSyntax
> import MachineLangSyntax

> uotoi Not = Instr (PUSH 1) 
>           $ Instr (JUMP 2) 
>           $ Instr (PUSH 0) 
>           $ Instr (JMPZ 3) 
>           $ Instr CMP 
>           $ Instr (PUSH 0) 
>           $ EmptyInstr

> uotoi Neg = Instr NEG EmptyInstr


> botoi Equ = Instr (PUSH 1)
>           $ Instr (JUMP 2)  
>           $ Instr (PUSH 0)  
>           $ Instr (JMPZ 3)  
>           $ Instr CMP 
>           $ EmptyInstr 

> botoi Lt  = Instr (PUSH 1)
>           $ Instr (JUMP 2)  
>           $ Instr (PUSH 0) 
>           $ Instr (JMPZ 3)
>           $ Instr CMP 
>           $ Instr (PUSH (negate 1)) 
>           $ Instr CMP
>           $ EmptyInstr 


> botoi Div = Instr DIV EmptyInstr

> botoi Or  = Instr ADD EmptyInstr
> botoi And = Instr MUL EmptyInstr
> botoi Add = Instr ADD EmptyInstr 
> botoi Sub = Instr SUB EmptyInstr
> botoi Mul = Instr MUL EmptyInstr
> botoi Mod = Instr MOD EmptyInstr

> $(attLabels [("generate", ''Instrs)])

> generateE_asp
>   =  (syn generate p_Val $
>      do val <- ter ch_val
>         return $ Instr (PUSH $ vtoi val) EmptyInstr)

>  .+: (syn generate p_Var $
>      do var <- ter ch_var
>         return $ Instr (LOAD var) EmptyInstr)

>  .+: (syn generate p_Bop $ 
>      do l    <- at ch_leftBop generate
>         op   <- ter ch_bop
>         r    <- at ch_rightBop generate
>         return $ concatInstrs (botoi op) (concatInstrs l r))

>  .+: (syn generate p_Uop $
>      do op <- ter ch_uop
>         i  <- at ch_expr generate
>         return $ concatInstrs (uotoi op) i)
>  .+: emptyAspect


> generateExpr e = (sem_Expr generateE_asp e emptyAtt) #. generate

> test1 = generateExpr (Bop (Bop (Val $ VInt 31) Equ (Val $ VInt 31) ) Or (Uop Not (Val $ VBool True)))
> -- getStack ExprCodeGenerator.test1 = [1] 
> test2 = generateExpr (Bop (Bop (Val $ VInt 31) Sub (Val $ VInt 31) ) Mul (Uop Neg (Bop (Bop (Val $ VInt 2) Add (Val $ VInt 2) ) Mul (Uop Neg (Val $ VInt 1)))))
> -- getStack ExprCodeGenerator.test2 = [0]
> test3 = generateExpr (Bop (Bop (Val $ VInt 30) Sub (Val $ VInt 31) ) Add (Uop Neg (Bop (Bop (Val $ VInt 2) Add (Val $ VInt 2) ) Mul (Uop Neg (Val $ VInt 1)))))
> -- getStack ExprCodeGenerator.test3 = [3]
> test4 = generateExpr (Bop (Bop (Val $ VInt 30) Sub (Val $ VInt 31) ) Add (Uop Neg (Bop (Bop (Val $ VInt 2) Add (Val $ VInt 2) ) Add (Uop Neg (Val $ VInt 1)))))
> -- getStack ExprCodeGenerator.test4 = [-4]
> test5 = generateExpr (Bop (Val $ VInt 1) Lt (Val $ VInt 2) )
> -- getStack ExprCodeGenerator.test4 = [1]
> test6 = generateExpr (Bop (Val $ VInt 1) Lt (Val $ VInt 1) )
> -- getStack ExprCodeGenerator.test4 = [0]
> test7 = generateExpr (Bop (Val $ VInt 1) Lt (Val $ VInt 0) )
> -- getStack ExprCodeGenerator.test4 = [0]
