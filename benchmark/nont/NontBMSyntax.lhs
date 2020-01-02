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


> module NontBMSyntax where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH

Expr definition

> $(addNont "Nont0")
> $(addNont "Nont1")
> $(addNont "Nont2")
> $(addNont "Nont3")
> $(addNont "Nont4")
> $(addNont "Nont5")
> $(addNont "Nont6")
> $(addNont "Nont7")
> $(addNont "Nont8")
> $(addNont "Nont9")
> $(addNont "Nont10")
> $(addNont "Nont11")
> $(addNont "Nont12")
> $(addNont "Nont13")
> $(addNont "Nont14")
> $(addNont "Nont15")
> {-$(addNont "Nont16")
> $(addNont "Nont17")
> $(addNont "Nont18")
> $(addNont "Nont19")-}

> $(addProd "Nont0_1" ''Nt_Nont0
>   [("ch0_1_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont0_2" ''Nt_Nont0 [] )

> $(addProd "Nont1_1" ''Nt_Nont1
>   [("ch1_1_1",  NonTer ''Nt_Nont2)])

> $(addProd "Nont2_1" ''Nt_Nont2
>   [("ch2_1_1",  NonTer ''Nt_Nont3)])

> $(addProd "Nont3_1" ''Nt_Nont3
>   [("ch3_1_1",  NonTer ''Nt_Nont4)])

> $(addProd "Nont4_1" ''Nt_Nont4
>   [("ch4_1_1",  NonTer ''Nt_Nont5)])

> $(addProd "Nont5_1" ''Nt_Nont5
>   [("ch5_1_1",  NonTer ''Nt_Nont6)])

> $(addProd "Nont6_1" ''Nt_Nont6
>   [("ch6_1_1",  NonTer ''Nt_Nont7)])

> $(addProd "Nont7_1" ''Nt_Nont7
>   [("ch7_1_1",  NonTer ''Nt_Nont8)])

> $(addProd "Nont8_1" ''Nt_Nont8
>   [("ch8_1_1",  NonTer ''Nt_Nont9)])

> $(addProd "Nont9_1" ''Nt_Nont9
>   [("ch9_1_1",  NonTer ''Nt_Nont10)])

> $(addProd "Nont10_1" ''Nt_Nont10
>   [("ch10_1_1",  NonTer ''Nt_Nont11)])

> $(addProd "Nont11_1" ''Nt_Nont11
>   [("ch11_1_1",  NonTer ''Nt_Nont12)])

> $(addProd "Nont12_1" ''Nt_Nont12
>   [("ch12_1_1",  NonTer ''Nt_Nont13)])

> $(addProd "Nont13_1" ''Nt_Nont13
>   [("ch13_1_1",  NonTer ''Nt_Nont14)])

> $(addProd "Nont14_1" ''Nt_Nont14
>   [("ch14_1_1",  NonTer ''Nt_Nont15)])

> $(addProd "Nont15_1" ''Nt_Nont15
>   [("ch15_1_1",  NonTer ''Nt_Nont0)])

> {-$(addProd "Nont16_1" ''Nt_Nont16
>   [("ch16_1_1",  NonTer ''Nt_Nont17)])

> $(addProd "Nont17_1" ''Nt_Nont17
>   [("ch17_1_1",  NonTer ''Nt_Nont18)])

> $(addProd "Nont18_1" ''Nt_Nont18
>   [("ch18_1_1",  NonTer ''Nt_Nont19)])

> $(addProd "Nont19_1" ''Nt_Nont19
>   [("ch19_1_1",  NonTer ''Nt_Nont1)])-}

> $(closeNTs [''Nt_Nont0, ''Nt_Nont1, ''Nt_Nont2, ''Nt_Nont3, ''Nt_Nont4, ''Nt_Nont5, ''Nt_Nont6, ''Nt_Nont7, ''Nt_Nont8, ''Nt_Nont9,
>  ''Nt_Nont10, ''Nt_Nont11, ''Nt_Nont12, ''Nt_Nont13, ''Nt_Nont14, ''Nt_Nont15]){-, ''Nt_Nont16, ''Nt_Nont17, ''Nt_Nont18, ''Nt_Nont19-}

> $(mkSemFuncs [''Nt_Nont0, ''Nt_Nont1, ''Nt_Nont2, ''Nt_Nont3, ''Nt_Nont4, ''Nt_Nont5, ''Nt_Nont6, ''Nt_Nont7, ''Nt_Nont8, ''Nt_Nont9,
>  ''Nt_Nont10, ''Nt_Nont11, ''Nt_Nont12, ''Nt_Nont13, ''Nt_Nont14, ''Nt_Nont15]){-, ''Nt_Nont16, ''Nt_Nont17, ''Nt_Nont18, ''Nt_Nont19-}
