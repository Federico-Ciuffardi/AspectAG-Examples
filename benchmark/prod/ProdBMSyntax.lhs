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


> module ProdBMSyntax where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH

Expr definition

> $(addNont "Nont1")

> $(addProd "Nont1_1" ''Nt_Nont1
>   [("ch1_1_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_2" ''Nt_Nont1 [] )

> $(addProd "Nont1_3" ''Nt_Nont1
>   [("ch1_3_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_4" ''Nt_Nont1
>   [("ch1_4_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_5" ''Nt_Nont1
>   [("ch1_5_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_6" ''Nt_Nont1
>   [("ch1_6_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_7" ''Nt_Nont1
>   [("ch1_7_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_8" ''Nt_Nont1
>   [("ch1_8_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_9" ''Nt_Nont1
>   [("ch1_9_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_10" ''Nt_Nont1
>   [("ch1_10_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_11" ''Nt_Nont1
>   [("ch1_11_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_12" ''Nt_Nont1
>   [("ch1_12_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_13" ''Nt_Nont1
>   [("ch1_13_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_14" ''Nt_Nont1
>   [("ch1_14_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_15" ''Nt_Nont1
>   [("ch1_15_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_16" ''Nt_Nont1
>   [("ch1_16_1",  NonTer ''Nt_Nont1)])

> $(addProd "Nont1_17" ''Nt_Nont1
>   [("ch1_17_1",  NonTer ''Nt_Nont1)])

> $(closeNTs [''Nt_Nont1])

> $(mkSemFuncs [''Nt_Nont1])
