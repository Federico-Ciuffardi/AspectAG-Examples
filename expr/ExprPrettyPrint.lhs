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

> bopPrecedence Mul = 4
> bopPrecedence Div = 4
> bopPrecedence Mod = 4
> bopPrecedence And = 4

> bopPrecedence Add = 3
> bopPrecedence Sub = 3
> bopPrecedence Or  = 3

> bopPrecedence Equ = 2
> bopPrecedence Lt  = 2

> $(attLabels [("pPrint", ''String), ("precedence", ''Int)])

> precedence_asp
>   =  (inh precedence p_Bop ch_leftBop $
>      do op <- ter ch_bop
>         return (bopPrecedence op))
>  .+: (inh precedence p_Bop ch_rightBop $
>      do op <- ter ch_bop
>         return (bopPrecedence op))
>  .+: inh precedence p_Uop ch_expr (return 5)
>  .+: emptyAspect

> pPrint_asp
>   =  syn pPrint p_Val (show <$> ter ch_val)
>  .+: syn pPrint p_Var (ter ch_var)
>  .+: (syn pPrint p_Bop $ 
>      do l    <- at ch_leftBop pPrint
>         op   <- ter ch_bop
>         r    <- at ch_rightBop pPrint
>         pred <- at lhs precedence
>         let s = l ++ " " ++ show op ++ " " ++ r
>         return $ if pred > bopPrecedence op then "(" ++ s ++ ")" else s )
>  .+: (syn pPrint p_Uop $
>      do op <- ter ch_uop
>         e  <- at ch_expr pPrint
>         return $ show op ++ e)
>  .+: precedence_asp


> pPrintExpr e = (sem_Expr pPrint_asp e (precedence =. 0 *. emptyAtt)) #. pPrint

> test1 = pPrintExpr (Bop (Bop (Var "x") Equ (Val $ VInt 31) ) Or (Uop Not (Val $ VBool True)))
> test2 = pPrintExpr (Bop (Bop (Var "x") Add (Val $ VInt 31) ) Mul (Uop Neg (Bop (Bop (Val $ VInt 2) Add (Val $ VInt 2) ) Mul (Uop Neg (Val $ VInt 1)))))
> test3 = pPrintExpr (Bop (Bop (Var "x") Add (Val $ VInt 31) ) Add (Uop Neg (Bop (Bop (Val $ VInt 2) Add (Val $ VInt 2) ) Mul (Uop Neg (Val $ VInt 1)))))
> test4 = pPrintExpr (Bop (Bop (Var "x") Add (Val $ VInt 31) ) Add (Uop Neg (Bop (Bop (Val $ VInt 2) Add (Val $ VInt 2) ) Add (Uop Neg (Val $ VInt 1)))))
