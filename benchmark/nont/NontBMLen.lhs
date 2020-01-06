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

> module NontBMLen where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import NontBMSyntax

> $(attLabels [("len", ''Int)])

> len_asp
>   =  (syn len p_Nont0_1 $
>      do i <- at ch_ch0_1_1 len
>         return (i+1))
>  .+: syn len p_Nont0_2 (return 0)
>  .+: (syn len p_Nont1_1 $
>      do i <- at ch_ch1_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont2_1 $
>      do i <- at ch_ch2_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont3_1 $
>      do i <- at ch_ch3_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont4_1 $
>      do i <- at ch_ch4_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont5_1 $
>      do i <- at ch_ch5_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont6_1 $
>      do i <- at ch_ch6_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont7_1 $
>      do i <- at ch_ch7_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont8_1 $
>      do i <- at ch_ch8_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont9_1 $
>      do i <- at ch_ch9_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont10_1 $
>      do i <- at ch_ch10_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont11_1 $
>      do i <- at ch_ch11_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont12_1 $
>      do i <- at ch_ch12_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont13_1 $
>      do i <- at ch_ch13_1_1 len
>         return (i+1))
>  {-.+: (syn len p_Nont14_1 $
>      do i <- at ch_ch14_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont15_1 $
>      do i <- at ch_ch15_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont16_1 $
>      do i <- at ch_ch16_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont17_1 $
>      do i <- at ch_ch17_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont18_1 $
>      do i <- at ch_ch18_1_1 len
>         return (i+1))
>  .+: (syn len p_Nont19_1 $
>      do i <- at ch_ch19_1_1 len
>         return (i+1)) -}
>  .+: emptyAspect

> nontLength nt = (sem_Nont0 len_asp nt emptyAtt) #. len

