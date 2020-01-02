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
>   =  (syn len p_Nont1_1 $
>      do i <- at ch_ch1_1_1 len
>         return (i+1))
>  .+: syn len p_Nont1_2 (return 0)
>  .+: (syn len p_Nont1_3 $
>      do i <- at ch_ch1_3_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_4 $
>      do i <- at ch_ch1_4_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_5 $
>      do i <- at ch_ch1_5_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_6 $
>      do i <- at ch_ch1_6_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_7 $
>      do i <- at ch_ch1_7_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_8 $
>      do i <- at ch_ch1_8_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_9 $
>      do i <- at ch_ch1_9_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_10 $
>      do i <- at ch_ch1_10_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_11 $
>      do i <- at ch_ch1_11_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_12 $
>      do i <- at ch_ch1_12_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_13 $
>      do i <- at ch_ch1_13_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_14 $
>      do i <- at ch_ch1_14_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_15 $
>      do i <- at ch_ch1_15_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_16 $
>      do i <- at ch_ch1_16_1 len
>         return (i+1))
>  .+: (syn len p_Nont1_17 $
>      do i <- at ch_ch1_17_1 len
>         return (i+1))
>  .+: emptyAspect

> nontLength nt = (sem_Nont1 len_asp nt emptyAtt) #. len

