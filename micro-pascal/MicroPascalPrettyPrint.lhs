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
> import MicroPascalSyntax

Example: pretty printing expressions

> $(attLabel "sshow" ''String)

> showExpr :: Expr -> String
> showExpr e = (sem_Expr asp_showExpr e EmptyAtt) #. sshow

> asp_showExpr
>  =   (syn sshow p_Val $ show <$> ter ch_val)
>  .+: (syn sshow p_Var  $ ter ch_var)
>  .+: (syn sshow p_Bop $
>      do l  <- at ch_leftBop sshow
>         op <- ter ch_bop
>         r  <- at ch_rightBop sshow
>         let wrap s = " " ++ s ++ " "
>         return $ "(" ++ l ++ wrap (show op) ++ r ++ ")"
>      )
>  .+: (syn sshow p_Uop $
>      do op <- ter ch_uop
>         e  <- at ch_expr sshow
>         return $ "(" ++ show op ++ " " ++ e ++ ")"
>      )
>  .+: emptyAspect

> test = showExpr (Bop (Var "x") Or (Uop Not (Val (VBool True))))
