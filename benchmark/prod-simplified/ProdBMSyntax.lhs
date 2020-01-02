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

> $(addProd "Nont1_1" ''Nt_Nont1 [] )

> $(addProd "Nont1_2" ''Nt_Nont1 [] )

> $(addProd "Nont1_3" ''Nt_Nont1 [] )

> $(addProd "Nont1_4" ''Nt_Nont1 [] )

> $(addProd "Nont1_5" ''Nt_Nont1 [] )

> $(addProd "Nont1_6" ''Nt_Nont1 [] )

> $(addProd "Nont1_7" ''Nt_Nont1 [] )

> $(addProd "Nont1_8" ''Nt_Nont1 [] )

> $(addProd "Nont1_9" ''Nt_Nont1 [] )

> $(addProd "Nont1_10" ''Nt_Nont1 [] )

> $(addProd "Nont1_11" ''Nt_Nont1 [] )

> $(addProd "Nont1_12" ''Nt_Nont1 [] )

> $(addProd "Nont1_13" ''Nt_Nont1 [] )

> $(addProd "Nont1_14" ''Nt_Nont1 [] )

> $(addProd "Nont1_15" ''Nt_Nont1 [] )

> $(addProd "Nont1_16" ''Nt_Nont1 [] )

> $(addProd "Nont1_17" ''Nt_Nont1 [] )


> $(closeNTs [''Nt_Nont1])

> $(mkSemFuncs [''Nt_Nont1])
