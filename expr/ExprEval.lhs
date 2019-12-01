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
> import qualified Prelude
> import Prelude hiding (or,and)
> import Data.Maybe
> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH

val operations

> instance Num Val where
>    a + b = VInt (toInteger a + toInteger b)
>    a * b = VInt (toInteger a * toInteger b)
>    a - b = VInt (toInteger a - toInteger b)
>    negate a = VInt (negate (toInteger a))
>    fromInteger i = VInt i
> instance Integral Val where
>    a `div` b = VInt (toInteger a `div` toInteger b)
>    a `mod` b = VInt (toInteger a `mod` toInteger b)
>    toInteger (VInt i) = i
>    toInteger a = error (show(a) ++ " is not of type VInt")
> instance Real Val where
>    toRational a = toRational (toInteger a)
> instance Ord Val where
>    VInt  i <= VInt j   = i <= j
>    VInt  i <= a        = error (show(a) ++ " is not of type VInt")
>    VBool b <= VBool b' = b <= b'
>    VBool b <= a        = error (show(a) ++ " is not of type VBool")
> instance Enum Val where

> VBool False `or` VBool False = VBool False
> VBool _ `or` VBool _ = VBool True
> VBool _ `or` a = error (show(a) ++ " is not type VBool")
> a `or` _ = error (show(a) ++ " is not type VBool")

> VBool True `and` VBool True = VBool True
> VBool _ `and` VBool _ = VBool False
> VBool _ `and` a = error (show(a) ++ " is not type VBool")
> a `and` _ = error (show(a) ++ " is not type VBool")

> no (VBool b) = VBool (not b)

eval definition

> type VarVals = [(String, Val)]

> $(attLabels [("eval", ''Val), ("env", ''VarVals)])

> eval_val = syn eval p_Val (ter ch_val)
> eval_var = syn eval p_Var (do e <- at lhs env
>                               x <- ter ch_var
>                               return (fromJust $ lookup x e))

> eval_Bop = syn eval p_Bop (do l  <- at ch_leftBop eval
>                               r  <- at ch_rightBop eval
>                               op <- ter ch_bop
>                               return (case op of Or  -> (l `or` r)
>                                                  And -> (l `and` r)
>                                                  Equ -> VBool (l == r)
>                                                  Lt  -> VBool (l < r)
>                                                  Add -> (l + r)
>                                                  Sub -> (l - r)
>                                                  Mul -> (l * r)
>                                                  Div -> (l `div` r)
>                                                  Mod -> (l `mod` r)))

> eval_Uop = syn eval p_Uop (do e  <- at ch_expr eval
>                               op <- ter ch_uop
>                               return (case op of Not -> no e
>                                                  Neg -> negate e))

> asp_eval = eval_Uop .+: eval_Bop .+: eval_val .+: eval_var .+: emptyAspect

> env_bop_l = inh env p_Bop ch_leftBop (at lhs env)
> env_bop_r = inh env p_Bop ch_rightBop (at lhs env)
> env_uop = inh env p_Uop ch_expr (at lhs env)

> asp_expr =   env_uop .+: env_bop_l .+: env_bop_r .+: asp_eval

> evalExpr exp envi = sem_Expr asp_expr exp  (env =. envi *. emptyAtt) #. eval