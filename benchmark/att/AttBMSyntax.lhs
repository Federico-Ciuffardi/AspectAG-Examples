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


> module AttBMSyntax where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH

Expr definition

> $(addNont "Nont1")

> $(addProd "Nont1_1" ''Nt_Nont1 [] )

> $(closeNTs [''Nt_Nont1])

> $(mkSemFuncs [''Nt_Nont1])
