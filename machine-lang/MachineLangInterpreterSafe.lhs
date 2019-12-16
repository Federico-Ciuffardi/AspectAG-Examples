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

> module MachineLangInterpreter where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import Data.Maybe
> import MachineLangSyntax

aux funcs for stack

> cmp i j | i > j     = 1
>         | i == j    = 0
>         | otherwise = (negate 1)

> updateStack NEG mStack _ = 
>   do h:tail <- mStack
>      return $ negate h:tail

> updateStack ADD mStack _ = 
>   do h:h':tail <- mStack
>      return $ h + h':tail

> updateStack SUB mStack _ = 
>   do h:h':tail <- mStack
>      return $ h - h':tail

> updateStack MUL mStack _ = 
>   do h:h':tail <- mStack
>      return $ h * h':tail

> updateStack DIV mStack _ = 
>   do h:h':tail <- mStack
>      return $ h `div` h':tail

> updateStack MOD mStack _ = 
>   do h:h':tail <- mStack
>      return $ h `mod` h':tail

> updateStack CMP mStack _ = 
>   do h:h':tail <- mStack
>      return $ h `cmp` h':tail

> updateStack (PUSH i) mStack  _ =
>   do stack <- mStack
>      return $ i:stack

> updateStack (JUMP i) stack  _ = stack

> updateStack (JMPZ i) mStack _ = 
>   do h:tail <- mStack
>      return $ tail

> updateStack (LOAD  var) mStack mEnv = 
>   do stack <- mStack
>      env   <- mEnv 
>      putStrLn $ "env on load1: " ++show env  
>      env   <- mEnv 
>      putStrLn $ "env on load2: " ++show env  
>      return $ (fromJust $ lookup var env):stack

> updateStack (STORE var) mStack _ = 
>   do h:tail <- mStack
>      return $ tail

> updateStack READ mStack _ = 
>   do stack <- mStack
>      ln    <- getLine 
>      return $ (read ln :: Integer) : stack

> updateStack WRITE mStack _ = 
>   do h:tail <- mStack
>      putStrLn $ show h
>      return $ tail

> updateStack SKIP stack _           = stack

aux funcs for env

> updateEnvAux (STORE var) (val:_) []                 = [(var,val)]
> updateEnvAux (STORE var) (val:_) ((var',val'):tail) | var == var' = (var,val):tail
>                                                     | otherwise   = (var',val') : updateEnvAux (STORE var) [val] tail
> updateEnvAux _ _ env = env

> updateEnv inst mStack mEnv =
>   do stack <- mStack
>      env <- mEnv   
>      return $ updateEnvAux inst stack env

aux funcs for skip

> decM mSkip =
>   do skip <- mSkip
>      return $ dec skip

> dec skip | skip <= 0    = 0
>          | otherwise    = skip - 1

> updateSkip (JUMP i) _      _  = return i
> updateSkip (JMPZ i) mStack mSkip  =
>   do h:_   <- mStack
>      skip  <- mSkip
>      return (if h == 0 then i else dec skip)
> updateSkip _ _ mSkip  =
>   do skip  <- mSkip
>      return (dec skip)

> ifSkip mSkip mThen mElse =
>   do skip <- mSkip
>      if skip - 1 <= 0 then mThen else mElse

> type Stack   = IO [Integer]
> type VarVals = IO [(String, Integer)]
> type Skip    = IO Int

> $(attLabels [("stack",''Stack), ("env",''VarVals), ("skip",''Skip)])

> skip_asp
>   =  syn skip p_EmptyInstr (return $ return 0)
>  .+: (syn skip p_Instr  $
>      do headInstr    <- ter ch_headInstr
>         currentStack <- at ch_tailInstrList stack
>         currentSkip  <- at ch_tailInstrList skip
>         return $ ifSkip currentSkip (updateSkip headInstr currentStack currentSkip) (decM currentSkip))
>  .+: emptyAspect

> env_asp  
>   =  syn env p_EmptyInstr (return $ return [])
>  .+: (syn env p_Instr  $
>      do headInstr    <- ter ch_headInstr
>         currentSkip  <- at ch_tailInstrList skip
>         currentEnv   <- at ch_tailInstrList env
>         currentStack <- at ch_tailInstrList stack
>         return $ ifSkip currentSkip (updateEnv headInstr currentStack currentEnv) currentEnv)
>  .+: skip_asp

> stack_asp  
>   =  syn stack p_EmptyInstr (return $ return[])
>  .+: (syn stack p_Instr  $
>      do headInstr    <- ter ch_headInstr
>         currentSkip  <- at ch_tailInstrList skip
>         currentEnv   <- at ch_tailInstrList env
>         currentStack <- at ch_tailInstrList stack
>         return $ ifSkip currentSkip (updateStack headInstr currentStack currentEnv) currentStack)
>  .+: env_asp

> getStack e = sem_Instrs stack_asp e emptyAtt #. stack

getStack tests

> test1_1 = getStack 
>   $ Instr (PUSH 1) 
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 0) 
>   $ Instr (JMPZ 3) 
>   $ Instr CMP 
>   $ Instr (PUSH 0) 
>   $ Instr (PUSH 0) 
>   $ EmptyInstr
> -- [1]
> test1_2 = getStack 
>   $ Instr (PUSH 1) 
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 0) 
>   $ Instr (JMPZ 3) 
>   $ Instr CMP 
>   $ Instr (PUSH 0) 
>   $ Instr (PUSH 1) 
>   $ EmptyInstr
> -- [0]
> test2 = getStack 
>   $ Instr NEG 
>   $ Instr (PUSH 30) 
>   $ EmptyInstr
> -- [-30]
> test3_1 = getStack 
>   $ Instr (PUSH 1)
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 0)
>   $ Instr (JMPZ 3)  
>   $ Instr CMP 
>   $ Instr (PUSH 0) 
>   $ Instr (PUSH 0) 
>   $ EmptyInstr
> -- [1] 
> test3_2 = getStack 
>   $ Instr (PUSH 1)
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 0)
>   $ Instr (JMPZ 3)  
>   $ Instr CMP 
>   $ Instr (PUSH 1) 
>   $ Instr (PUSH 0) 
>   $ EmptyInstr
> -- [0] 
> test4 = getStack 
>   $ Instr DIV 
>   $ Instr (PUSH 31) 
>   $ Instr (PUSH 2) 
>   $ EmptyInstr
> -- [15]
> test5 = getStack 
>   $ Instr ADD 
>   $ Instr (PUSH 5) 
>   $ Instr (PUSH 5) 
>   $ EmptyInstr
> -- [10]
> test6 = getStack 
>   $ Instr MUL 
>   $ Instr (PUSH 5) 
>   $ Instr (PUSH 5) 
>   $ EmptyInstr
> -- [25]
> test7 = getStack 
>   $ Instr SUB 
>   $ Instr (PUSH 5) 
>   $ Instr (PUSH 2) 
>   $ EmptyInstr
> -- [3]
> test8 = getStack 
>   $ Instr MOD 
>   $ Instr (PUSH 31) 
>   $ Instr (PUSH 2) 
>   $ EmptyInstr
> -- [1]
> test9_1 = getStack 
>   $ Instr SUB 
>   $ Instr (LOAD "x") 
>   $ Instr (PUSH 2) 
>   $ Instr (STORE "x") 
>   $ Instr (PUSH 5) 
>   $ EmptyInstr
> -- [3]
> test9_2 = getStack 
>   $ Instr SUB 
>   $ Instr (LOAD "x") 
>   $ Instr (PUSH 2) 
>   $ Instr (STORE "x") 
>   $ Instr (PUSH 5) 
>   $ Instr (STORE "x") 
>   $ Instr (PUSH 22) 
>   $ EmptyInstr
> -- [3]

> test10 = getStack 
>   $ Instr WRITE 
>   $ Instr SUB 
>   $ Instr (LOAD "x") 
>   $ Instr (PUSH 2) 
>   $ Instr (STORE "x") 
>   $ Instr (PUSH 5) 
>   $ EmptyInstr
> -- 3
> -- []

> test11 = getStack 
>   $ Instr (LOAD "x") 
>   $ Instr (STORE "x") 
>   $ Instr READ 
>   $ EmptyInstr
> -- [val]

> test12 = getStack 
>   $ Instr WRITE 
>   $ Instr (LOAD "x") 
>   $ Instr (STORE "x") 
>   $ Instr READ
>   $ Instr (JUMP 3)
>   $ Instr (STORE "x") 
>   $ Instr (PUSH 0) 
>   $ Instr (JMPZ 4) 
>   $ Instr (PUSH 0) 
>   $ EmptyInstr
> -- [val]