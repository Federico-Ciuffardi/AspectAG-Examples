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

> module ExprEval where

> import ExprSyntax
> import Data.Maybe
> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH

datas

> data Error = Expected Type Type
>   deriving (Eq)
> instance Show Error where
>  show (Expected    ty ty') = "Expected: " ++ show ty ++ " Actual: " ++ show ty'


> data ExceptlType = Ok Type | Fail [Error]
>   deriving(Show,Eq)

> type VarTypes = [(String, Type)]

aux functions

> isFail (Fail _) = True
> isFail _        = False

> fromOk (Ok a) = a
> fromOk _      = error "Expected OK" 

> checkOut expected actual out | isFail actual      = actual
>                              | isFail expected    = expected
>                              | expected == actual = out
>                              | otherwise          = Fail [Expected (fromOk expected) (fromOk actual)]

> check expected actual = checkOut expected actual actual

> merge (Fail f) (Fail f') _ = Fail (f ++ f')
> merge (Fail f) (Ok _)    _ = Fail f 
> merge (Ok _) (Fail f)    _ = Fail f 
> merge (Ok a) (Ok b)      c | a == b    = c
>                            | otherwise = Fail [Expected a b]

> checkTypeBop eT lT rT outT = merge (check eT lT) (check eT rT) outT

> checkTypeUop eT t tOut = checkOut eT t tOut

defining expected types for each operator

> typeCheckUop Not t = checkTypeUop (Ok TBool) t (Ok TBool)
> typeCheckUop Neg t = checkTypeUop (Ok TInt) t (Ok TInt)

> typeCheckBop And tL tR = checkTypeBop (Ok TBool) tL tR (Ok TBool)
> typeCheckBop Or tL tR = checkTypeBop (Ok TBool) tL tR (Ok TBool)

> typeCheckBop Equ tL tR = checkTypeBop (Ok TInt) tL tR (Ok TBool)
> typeCheckBop Lt tL tR = checkTypeBop (Ok TInt) tL tR (Ok TBool)

> typeCheckBop Add tL tR = checkTypeBop (Ok TInt) tL tR (Ok TInt)
> typeCheckBop Sub tL tR = checkTypeBop (Ok TInt) tL tR (Ok TInt)
> typeCheckBop Mul tL tR = checkTypeBop (Ok TInt) tL tR (Ok TInt)
> typeCheckBop Div tL tR = checkTypeBop (Ok TInt) tL tR (Ok TInt)
> typeCheckBop Mod tL tR = checkTypeBop (Ok TInt) tL tR (Ok TInt)

attributes definitions

> $(attLabels [("exprType", ''ExceptlType), ("varTypes", ''VarTypes)])

> varTypes_asp 
>   =  inh varTypes p_Bop ch_leftBop (at lhs varTypes)
>  .+: inh varTypes p_Bop ch_rightBop (at lhs varTypes)
>  .+: inh varTypes p_Uop ch_expr (at lhs varTypes)
>  .+: emptyAspect

> exprType_asp
>   =  syn exprType p_Val (do val <- ter ch_val
>                             return (Ok $ typeof val))

>  .+: syn exprType p_Var (do varType <- at lhs varTypes
>                             varName <- ter ch_var
>                             return (Ok $ fromJust $ lookup varName varType))

>  .+: syn exprType p_Bop (do l  <- at ch_leftBop exprType
>                             r  <- at ch_rightBop exprType
>                             op <- ter ch_bop
>                             return (typeCheckBop op l r))

>  .+: syn exprType p_Uop (do ty <- at ch_expr exprType
>                             op <- ter ch_uop
>                             return (typeCheckUop op ty))
>  .+: varTypes_asp

> getExprType exp vT = sem_Expr exprType_asp exp (varTypes =. vT *. emptyAtt) #. exprType

tests

> ok1 = getExprType (Bop (Val $ VInt 5) Add (Bop (Val $ VInt 5) Add (Val $ VInt 5))) []             
> -- Ok integer
> ok2 = getExprType (Bop (Val $ VInt 5) Add (Uop Neg (Val $ VInt 5) )) []                           
> -- Ok integer
> ok3 = getExprType (Bop (Val $ VBool True) And (Bop (Val $ VInt 5) Equ (Var "x"))) [("x",TInt)]    
> -- Ok boolean
> fail1 = getExprType (Bop (Val $ VBool True) And (Bop (Val $ VInt 5) Equ (Var "x"))) [("x",TBool)] 
> -- Fail [Expected: integer Actual: boolean]
> fail2 = getExprType (Bop (Val $ VInt 5) And (Bop (Val $ VInt 5) And (Val $ VInt 5))) []          
> -- Fail [Expected: boolean Actual: integer,Expected: boolean Actual: integer,Expected: boolean Actual: integer]
> fail3 = getExprType (Uop Neg (Val $ VBool False) ) []
> -- Fail [Expected: integer Actual: boolean]