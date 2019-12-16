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
> import System.IO.Unsafe

aux funcs for stack

> updateStack NEG (h:tail) _         = (negate h):tail
> updateStack ADD (h:h':tail) _      = (h + h'):tail
> updateStack SUB (h:h':tail) _      = (h - h'):tail
> updateStack MUL (h:h':tail) _      = (h * h'):tail
> updateStack DIV (h:h':tail) _      = (h `div` h'):tail
> updateStack MOD (h:h':tail) _      = (h `mod` h'):tail
> updateStack CMP (h:h':tail) _      | h > h'     = 1:tail
>                                    | h == h'    = 0:tail
>                                    | otherwise  = (negate 1):tail
> updateStack (PUSH i) stack  _      = i:stack
> updateStack (JUMP i) stack  _      = stack
> updateStack (JMPZ i) (h:tail) _    = tail
> updateStack (LOAD  var) stack env  = (fromJust $ lookup var env):stack
> updateStack (STORE var) (h:tail) _ = tail
> updateStack READ stack _           = (read (unsafePerformIO getLine):: Integer):stack
> updateStack WRITE (h:tail) _       = tail
> updateStack SKIP stack _           = stack
> updateStack a b c = error ("Error: " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " ")

aux funcs for env

> updateEnv (STORE var) (val:_) []                 = [(var,val)]
> updateEnv (STORE var) (val:_) ((var',val'):tail) | var == var' = (var,val):tail
>                                                  | otherwise   = (var',val') : updateEnv (STORE var) [val] tail
> updateEnv _ _ env = env

aux funcs for skip

> updateSkip (JUMP i) _     = i
> updateSkip (JMPZ i) (h:_) | h == 0    = i
>                           | otherwise = 0
> updateSkip _ _            = 0

aux funcs for o

> updateOut WRITE (h:_) currentOut = do currentOut
>                                       putStrLn $ show h
> updateOut _ _  currentOut       = currentOut

> type Stack   = [Integer]
> type VarVals = [(String, Integer)]
> type Out      = IO ()

> $(attLabels [("stack",''Stack), ("env",''VarVals), ("skip",''Int), ("o",''Out)])

 env_bop_l = inh env p_Bop ch_leftBop (at lhs env)
 env_bop_r = inh env p_Bop ch_rightBop (at lhs env)
 env_uop = inh env p_Uop ch_expr (at lhs env)


> skip_asp
>   =  syn skip p_EmptyInstr (return 0)
>  .+: (syn skip p_Instr  $
>      do headInstr    <- ter ch_headInstr
>         currentStack <- at ch_tailInstrList stack
>         currentSkip  <- at ch_tailInstrList skip
>         return $  if currentSkip-1 <= 0 then updateSkip headInstr currentStack else currentSkip - 1)
>  .+: emptyAspect

> env_asp  
>   =  syn env p_EmptyInstr (return [])
>  .+: (syn env p_Instr  $
>      do headInstr    <- ter ch_headInstr
>         currentSkip  <- at ch_tailInstrList skip
>         currentEnv   <- at ch_tailInstrList env
>         currentStack <- at ch_tailInstrList stack
>         return $ if currentSkip-1 <= 0 then updateEnv headInstr currentStack currentEnv else currentEnv)
>  .+: skip_asp

> stack_asp  
>   =  syn stack p_EmptyInstr (return [])
>  .+: (syn stack p_Instr  $
>      do headInstr    <- ter ch_headInstr
>         currentSkip  <- at ch_tailInstrList skip
>         currentEnv   <- at ch_tailInstrList env
>         currentStack <- at ch_tailInstrList stack
>         return $  if currentSkip-1 <= 0 then updateStack headInstr currentStack currentEnv else currentStack)
>  .+: env_asp

> out_asp
>   =  syn o p_EmptyInstr (return (return ()))
>  .+: (syn o p_Instr  $
>      do headInstr    <- ter ch_headInstr
>         currentStack <- at ch_tailInstrList stack
>         currentSkip  <- at ch_tailInstrList skip
>         currentOut  <- at ch_tailInstrList o
>         return $  if currentSkip-1 <= 0 then updateOut headInstr currentStack currentOut  else do currentOut)
>  .+: stack_asp

> getStack e = sem_Instrs stack_asp e emptyAtt #. stack

> getOut e = sem_Instrs out_asp e emptyAtt #. o

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

> test10 = getOut 
>   $ Instr WRITE 
>   $ Instr SUB 
>   $ Instr (LOAD "x") 
>   $ Instr (PUSH 2) 
>   $ Instr (STORE "x") 
>   $ Instr (PUSH 5) 
>   $ EmptyInstr
> -- 3

> test11 = getStack 
>   $ Instr (LOAD "x") 
>   $ Instr (STORE "x") 
>   $ Instr READ 
>   $ EmptyInstr
> -- [val]

> test12 = getOut 
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
> -- val

> test13 = getOut 
>   $ Instr WRITE 
>   $ Instr (PUSH 1) 
>   $ Instr (JUMP 3) 
>   $ Instr WRITE 
>   $ Instr (PUSH 0) 
>   $ Instr (JMPZ 4) 
>   $ Instr (PUSH 1)  
>   $ EmptyInstr
> -- 0

> test14 = getOut 
>   $ Instr WRITE 
>   $ Instr (PUSH 1) 
>   $ Instr (JUMP 3) 
>   $ Instr WRITE 
>   $ Instr (PUSH 0) 
>   $ Instr (JMPZ 4) 
>   $ Instr (PUSH 0)  
>   $ EmptyInstr
> -- 1
