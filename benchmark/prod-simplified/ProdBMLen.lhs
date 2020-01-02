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

> module ProdBMLen where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import ProdBMSyntax

> $(attLabels [("len", ''Int)])

> len_asp
>   =  syn len p_Nont1_1 (return 0)
>  .+: syn len p_Nont1_2 (return 0)
>  .+: syn len p_Nont1_3 (return 0)
>  .+: syn len p_Nont1_4 (return 0)
>  .+: syn len p_Nont1_5 (return 0)
>  .+: syn len p_Nont1_6 (return 0)
>  .+: syn len p_Nont1_7 (return 0)
>  .+: syn len p_Nont1_8 (return 0)
>  .+: syn len p_Nont1_9 (return 0)
>  .+: syn len p_Nont1_10 (return 0)
>  .+: syn len p_Nont1_11 (return 0)
>  .+: syn len p_Nont1_12 (return 0)
>  .+: syn len p_Nont1_13 (return 0)
>  .+: syn len p_Nont1_14 (return 0)
>  .+: syn len p_Nont1_15 (return 0)
>  .+: syn len p_Nont1_16 (return 0)
>  .+: syn len p_Nont1_17 (return 0)
>  .+: emptyAspect

> nontLength nt = (sem_Nont1 len_asp nt emptyAtt) #. len

