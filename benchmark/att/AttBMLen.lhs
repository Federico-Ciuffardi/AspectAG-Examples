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

> module AttBMLen where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import AttBMSyntax

> $(attLabels [("len1", ''Int),("len2", ''Int),("len3", ''Int),("len4", ''Int),("len5", ''Int),("len6", ''Int),("len7", ''Int),("len8", ''Int),("len9", ''Int),("len10", ''Int),
>              ("len11", ''Int),("len12", ''Int),("len13", ''Int),("len14", ''Int),("len15", ''Int),("len16", ''Int)])

> len_asp
>   =  syn len1 p_Nont1_1 (return 0)
>  .+: syn len2 p_Nont1_1 (return 0)
>  .+: syn len3 p_Nont1_1 (return 0)
>  .+: syn len4 p_Nont1_1 (return 0)
>  .+: syn len5 p_Nont1_1 (return 0)
>  .+: syn len6 p_Nont1_1 (return 0)
>  .+: syn len7 p_Nont1_1 (return 0)
>  .+: syn len8 p_Nont1_1 (return 0)
>  .+: syn len9 p_Nont1_1 (return 0)
>  .+: syn len10 p_Nont1_1 (return 0)
>  .+: syn len11 p_Nont1_1 (return 0)
>  .+: syn len12 p_Nont1_1 (return 0)
>  .+: syn len13 p_Nont1_1 (return 0)
>  .+: syn len14 p_Nont1_1 (return 0)
>  .+: syn len15 p_Nont1_1 (return 0)
>  .+: syn len16 p_Nont1_1 (return 0)

>  .+: emptyAspect

> nontLength nt = (sem_Nont1 len_asp nt emptyAtt) #. len1

