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

> module ExprOptimize where

> import ExprSyntax
> import ExprEval
> import qualified Prelude
> import Prelude hiding (or,and)
> import Data.Maybe
> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH

Bop optimization

> -- Two vals
> optimizeBop (Val l) op (Val r) = Val (bopEval l op r)
> -- Add
> optimizeBop e Add (Val (VInt 0)) = e
> optimizeBop (Val (VInt 0)) Add e = e
> -- Mul
> optimizeBop _ Mul (Val (VInt 0)) = Val (VInt 0)
> optimizeBop (Val (VInt 0)) Mul _ = Val (VInt 0)

> optimizeBop e Mul (Val (VInt 1)) = e
> optimizeBop (Val (VInt 1)) Mul e = e
> -- And
> optimizeBop e And (Val (VBool True)) = e
> optimizeBop (Val (VBool True)) And e = e

> optimizeBop _ And (Val (VBool False)) = Val (VBool False)
> optimizeBop (Val (VBool False)) And _ = Val (VBool False)
> -- Or
> optimizeBop _ Or (Val (VBool True)) = Val (VBool True)
> optimizeBop (Val (VBool True)) Or _ = Val (VBool True)

> optimizeBop e Or (Val (VBool False)) = e
> optimizeBop (Val (VBool False)) Or e = e

> -- TO-DO the remaining operators optimizations

> --Otherwise
> optimizeBop l op r = Bop l op r

Uop optimization

> optimizeUop op (Val v) = Val (uopEval op v)
> optimizeUop op e = Uop op e

Expr optimization

> $(attLabels [("optimize", ''Expr)])

> optimize_asp
>   =  syn optimize p_Val (do v <- ter ch_val
>                             return (Val v))

>  .+: syn optimize p_Var (do v <- ter ch_var
>                             return (Var v))

  
>  .+: syn optimize p_Bop (do l  <- at ch_leftBop optimize
>                             r  <- at ch_rightBop optimize
>                             op <- ter ch_bop
>                             return (optimizeBop l op r))


>  .+: syn optimize p_Uop (do e  <- at ch_expr optimize
>                             op <- ter ch_uop
>                             return (optimizeUop op e))

>  .+: emptyAspect

> optimizeExpr exp = sem_Expr optimize_asp exp  emptyAtt #. optimize

Tests

> t    = Val $ VBool $ True
> f    = Val $ VBool $ False

> one  = Val $ VInt  $ 1
> zero = Val $ VInt  $ 0
> two  = Val $ VInt  $ 2

> x    = Var $ "x"

> test1 = optimizeExpr (Uop Not (Bop t And (Uop Not (Bop t Or f )) )) 
> --Val {val = True}
> test2 = optimizeExpr (Uop Neg one) 
> -- Val {val = -5}
> test3 = optimizeExpr (Uop Neg x) 
> -- Uop {uop = Neg, expr = Var {var = "x"}}
> test4 = optimizeExpr (Bop x And (Uop Not (Bop t And f )))
> -- Var {var = "x"}
> test5 = optimizeExpr (Bop x And (Bop t And f ))
> -- Val {val = False}
> test6 = optimizeExpr (Bop x Or (Uop Not (Bop t And f )))
> -- Val {val = True}
> test7 = optimizeExpr (Bop x Or (Bop t And f ))
> -- Var {var = "x"}
> test8 = optimizeExpr (Bop x Mul (Uop Neg (Bop one Sub one )))
> -- Val {val = 0}
> test9 = optimizeExpr (Bop x Mul (Uop Neg (Bop zero Sub one )))
> -- Var {var = "x"}
> test10 = optimizeExpr (Bop x Mul (Uop Neg (Bop two Sub one )))
> -- Bop {leftBop = Var {var = "x"}, bop = Mul, rightBop = Val {val = -1}}
> test11 = optimizeExpr (Bop x Add (Uop Neg (Bop one Sub one )))
> -- Var {var = "x"}
