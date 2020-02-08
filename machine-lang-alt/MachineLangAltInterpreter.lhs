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

> module MachineLangAltInterpreter where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import Data.Maybe
> import MachineLangSyntax
> import MachineLangAltSyntax
> import MachineLangAltGenerator

atts defs

> type Stack  = [Integer]
> type VarEnv = [(String, Integer)]
> type State = IO (Stack,VarEnv)

> $(attLabels [("state",''State), ("nextInstrs",''InstrsAlt)])

aux funcs for state

> updateState inst mState = 
>   do (stack,env) <- mState
>      i           <- processIO inst stack
>      let stack' = i++stack
>      return (updateStack inst stack' env,updateEnv inst stack' env)

aux funcs for IO 

> processIO READ _ = 
>   do ln    <- getLine 
>      return [read ln :: Integer]
> processIO WRITE (h:_) = 
>   do putStrLn $ show h
>      return []
> processIO _ _ = return []


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
> -- updateStack (JUMP i) stack  _      = stack
> updateStack (JMPZ i) (h:tail) _    = tail
> updateStack (LOAD  var) stack env  = (fromJust $ lookup var env):stack
> updateStack (STORE var) (h:tail) _ = tail
> updateStack READ stack _           = stack
> updateStack WRITE (h:tail) _       = tail
> updateStack SKIP stack _           = stack
> updateStack a b c = error ("updateStack error: " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " ")

aux funcs for jumps

> nextState currentnInstr currentState njnextInstrs jnextInstrs =
>   do (sHead:sTail,env) <- currentState
>      if sHead /= 0 then 
>        getState njnextInstrs (return (sTail,env))
>      else 
>        getState jnextInstrs (return (sTail,env))

 nextState (JMPZ i) mState _ prvInstrs nxtInstrs= 
   do (sHead:sTail,env) <- mState
      let mState' =  return (sTail,env)
      if sHead /= 0 then getState mState' prvInstrs'' (arrToInstrs nxtInstrs'') else getState mState' prvInstrs' (arrToInstrs nxtInstrs')
   where (prvInstrs', nxtInstrs')   = jump i prvInstrs nxtInstrs
         (prvInstrs'', nxtInstrs'') = jump 1 prvInstrs nxtInstrs
 nextState _        _ noJumpState _ _ = noJumpState

aux funcs for env

> updateEnv (STORE var) (val:_) []                 = [(var,val)]
> updateEnv (STORE var) (val:_) ((var',val'):tail) | var == var' = (var,val):tail
>                                                  | otherwise   = (var',val') : updateEnv (STORE var) [val] tail
> updateEnv _ _ env = env

> nextInstrs_asp  
>   =  syn nextInstrs p_EmptyInstrAlt (return $ EmptyInstrAlt)
>  .+: (syn nextInstrs p_InstrAlt  $
>        do headInstr  <- ter ch_headInstrAlt
>           tailInstrs <- at ch_tailInstrAltList nextInstrs
>           return $ InstrAlt headInstr tailInstrs)
>  .+: (syn nextInstrs p_JumpInstrAlt $
>        do currentnInstr <- ter ch_cond
>           njnextInstrs   <- at ch_njTailInstrList nextInstrs
>           jnextInstrs   <- at ch_jTailInstrList nextInstrs
>           return $ JumpInstrAlt currentnInstr njnextInstrs jnextInstrs) 
>  .+: prevInstrs_asp

> state_asp  
>   =  (inh state p_InstrAlt ch_tailInstrAltList $
>        do currentnInstr <- ter ch_headInstrAlt
>           currentState <- at lhs state
>           return $ updateState currentnInstr currentState)

>  .+: (inh state p_JumpInstrAlt ch_jTailInstrList $
>        do currentnInstr <- ter ch_cond
>           currentState <- at lhs state
>           return $ updateState currentnInstr currentState)
>  .+: (inh state p_JumpInstrAlt ch_njTailInstrList $
>        do currentnInstr <- ter ch_cond
>           currentState <- at lhs state
>           return $ updateState currentnInstr currentState)

>  .+: (syn state p_InstrAlt $ at ch_tailInstrAltList state)

>  .+: (syn state p_JumpInstrAlt $
>        do currentnInstr <- ter ch_cond
>           currentState  <- at lhs state
>           njnextInstrs   <- at ch_njTailInstrList nextInstrs
>           jnextInstrs   <- at ch_jTailInstrList nextInstrs
>           return $ nextState currentnInstr currentState njnextInstrs jnextInstrs) -- keep executing or jump

>  .+: syn state p_EmptyInstrAlt (at lhs state) -- end reached with no jumps
>  .+: nextInstrs_asp

> getState progAlt stt = sem_InstrsAlt state_asp progAlt (state =. stt  *. emptyAtt) #. state
> interpret prog = getState (generateAlt prog) (return ([],[]))


interpret tests

> test0 = interpret 
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 1) 
>   $ Instr (PUSH 2) 
>   $ Instr (PUSH 3) 
>   $ EmptyInstr
> -- ([3,2],[])
> test1_1 = interpret 
>   $ Instr (PUSH 9) 
>   $ Instr (PUSH 9) 
>   $ Instr CMP 
>   $ Instr (JMPZ 3) 
>   $ Instr (PUSH 0) 
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 1) 
>   $ EmptyInstr
> -- ([1],[])
> test1_2 = interpret 
>   $ Instr (PUSH 9) 
>   $ Instr (PUSH 8) 
>   $ Instr CMP 
>   $ Instr (JMPZ 3) 
>   $ Instr (PUSH 0) 
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 1) 
>   $ EmptyInstr
> -- ([0],[])
> test2 = interpret 
>   $ Instr (PUSH 30) 
>   $ Instr NEG 
>   $ EmptyInstr
> -- ([-30],[])
> test3_1 = interpret 
>   $ Instr (PUSH 0) 
>   $ Instr (PUSH 0) 
>   $ Instr CMP 
>   $ Instr (JMPZ 3)  
>   $ Instr (PUSH 0)
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 1)
>   $ EmptyInstr
> -- ([1],[])
> test3_2 = interpret 
>   $ Instr (PUSH 1) 
>   $ Instr (PUSH 0) 
>   $ Instr CMP 
>   $ Instr (JMPZ 3)  
>   $ Instr (PUSH 0)
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 1)
>   $ EmptyInstr
> -- ([0],[])
> test4 = interpret 
>   $ Instr (PUSH 2) 
>   $ Instr (PUSH 31) 
>   $ Instr DIV 
>   $ EmptyInstr
> -- ([15],[])
> test5 = interpret 
>   $ Instr (PUSH 5) 
>   $ Instr (PUSH 5) 
>   $ Instr ADD 
>   $ EmptyInstr
> -- ([10],[])
> test6 = interpret 
>   $ Instr (PUSH 5) 
>   $ Instr (PUSH 5) 
>   $ Instr MUL 
>   $ EmptyInstr
> -- ([25],[])
> test7 = interpret 
>   $ Instr (PUSH 2) 
>   $ Instr (PUSH 5) 
>   $ Instr SUB 
>   $ EmptyInstr
> -- ([3],[])
> test8 = interpret 
>   $ Instr (PUSH 2) 
>   $ Instr (PUSH 31) 
>   $ Instr MOD 
>   $ EmptyInstr
> -- ([1],[])
> test9_1 = interpret 
>   $ Instr (PUSH 5) 
>   $ Instr (STORE "x") 
>   $ Instr (PUSH 2) 
>   $ Instr (LOAD "x") 
>   $ Instr SUB 
>   $ EmptyInstr
> -- ([3],[("x",5)])
> test9_2 = interpret
>   $ Instr (PUSH 22) 
>   $ Instr (STORE "x") 
>   $ Instr (PUSH 5) 
>   $ Instr (STORE "x") 
>   $ Instr (PUSH 2) 
>   $ Instr (LOAD "x") 
>   $ Instr SUB 
>   $ EmptyInstr
> -- ([3],[("x",5)])
> test10 = interpret 
>   $ Instr (PUSH 5) 
>   $ Instr (STORE "x") 
>   $ Instr (PUSH 2) 
>   $ Instr (LOAD "x") 
>   $ Instr SUB 
>   $ Instr WRITE 
>   $ EmptyInstr
> -- 3
> -- ([],[("x",5)])
> test11 = interpret 
>   $ Instr READ 
>   $ Instr (STORE "x") 
>   $ Instr (LOAD "x") 
>   $ EmptyInstr
> -- ([userVal],[("x",userVal)])
> test12 = interpret 
>   $ Instr (PUSH 0) 
>   $ Instr (JMPZ 4) 
>   $ Instr (PUSH 0) 
>   $ Instr (STORE "x") 
>   $ Instr (JUMP 3)
>   $ Instr READ
>   $ Instr (STORE "x") 
>   $ Instr (LOAD "x") 
>   $ Instr WRITE 
>   $ EmptyInstr
> -- userVal
> -- ([userVal],[("x",userVal)])
> test13 = interpret 
>   $ Instr (PUSH 0) 
>   $ Instr (STORE "x") 
>   $ Instr (LOAD "x") 
>   $ Instr WRITE 
>   $ Instr (LOAD "x") 
>   $ Instr (PUSH 1) 
>   $ Instr ADD
>   $ Instr (STORE "x") 
>   $ Instr READ
>   $ Instr (JMPZ 2) 
>   $ Instr (JUMP (negate 8))
>   $ EmptyInstr
> -- i = 0
> -- repeat
> -- write i
> -- i++
> -- until read == 0 
> -- write i
> -- it should write 0,1,2..
> test14 = interpret 
>   $ Instr (PUSH 0) 
>   $ Instr (PUSH 1) 
>   $ Instr ADD
>   $ Instr (STORE "x") 
>   $ Instr (LOAD "x") 
>   $ Instr (LOAD "x") 
>   $ Instr (PUSH 10)
>   $ Instr CMP
>   $ Instr (PUSH 0) 
>   $ Instr CMP 
>   $ Instr (JMPZ 3)  
>   $ Instr (PUSH 0)
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 1)
>   $ Instr (JMPZ (negate 13)) 
>   $ Instr WRITE 
>   $ EmptyInstr
> -- 10
> -- ([],[("x",10)]) 
> test15 = interpret 
>   $ Instr (PUSH 0) 
>   $ Instr (PUSH 1) 
>   $ Instr ADD
>   $ Instr (STORE "x") 
>   $ Instr (LOAD "x") 
>   $ Instr WRITE 
>   $ Instr (LOAD "x") 
>   $ Instr (LOAD "x") 
>   $ Instr (PUSH 10)
>   $ Instr CMP
>   $ Instr (PUSH 0) 
>   $ Instr CMP 
>   $ Instr (JMPZ 3)  
>   $ Instr (PUSH 0)
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 1)
>   $ Instr (JMPZ (negate 15)) 
>   $ EmptyInstr
> -- 1
> -- 2
> -- 3
> -- 4
> -- 5
> -- 6
> -- 7
> -- 8
> -- 9
> -- 10
> -- ([10],[("x",10)])
> test16 = interpret 
>   $ Instr (PUSH 0) 
>   $ Instr (PUSH 1) 
>   $ Instr ADD
>   $ Instr (STORE "x") 
>   $ Instr (LOAD "x") 
>   $ Instr WRITE 
>   $ Instr (LOAD "x") 
>   $ Instr (LOAD "x") 
>   $ Instr (PUSH 10)
>   $ Instr CMP
>   $ Instr (JMPZ 2) 
>   $ Instr (JUMP (negate 10)) 
>   $ EmptyInstr
> -- 1
> -- 2
> -- 3
> -- 4
> -- 5
> -- 6
> -- 7
> -- 8
> -- 9
> -- 10
> -- ([10],[("x",10)])
> test17_1 = interpret 
>   $ Instr (PUSH 0) 
>   $ Instr WRITE 
>   $ Instr (PUSH 1) 
>   $ Instr WRITE 
>   $ Instr (PUSH 2) 
>   $ Instr WRITE 
>   $ Instr (PUSH 3) 
>   $ Instr WRITE 
>   $ EmptyInstr
> -- 0
> -- 1
> -- 2
> -- 3
> -- ([],[])
> test17_2 = interpret 
>   $ Instr (PUSH 2) 
>   $ Instr (JMPZ 2) 
>   $ Instr (PUSH 0) 
>   $ Instr WRITE 
>   $ Instr (PUSH 1) 
>   $ Instr WRITE 
>   $ Instr (PUSH 2) 
>   $ Instr WRITE 
>   $ Instr (PUSH 3) 
>   $ Instr WRITE 
>   $ EmptyInstr
> -- 0
> -- 1
> -- 2
> -- 3
> -- ([],[])
> test17_3 = interpret 
>   $ Instr (PUSH 0) 
>   $ Instr (JMPZ 3) 
>   $ Instr (PUSH 0) 
>   $ Instr WRITE 
>   $ Instr (PUSH 1) 
>   $ Instr WRITE 
>   $ Instr (PUSH 2) 
>   $ Instr WRITE 
>   $ Instr (PUSH 3) 
>   $ Instr WRITE 
>   $ EmptyInstr
> -- 1
> -- 2
> -- 3
> -- ([],[])
> test_iterate i = interpret 
>   $ Instr (PUSH 0) 
>   $ Instr (PUSH 1) 
>   $ Instr ADD
>   $ Instr (STORE "x") 
>   $ Instr (LOAD "x") 
>   $ Instr (LOAD "x") 
>   $ Instr (PUSH i)
>   $ Instr CMP
>   $ Instr (PUSH 0) 
>   $ Instr CMP 
>   $ Instr (JMPZ 3)  
>   $ Instr (PUSH 0)
>   $ Instr (JUMP 2) 
>   $ Instr (PUSH 1)
>   $ Instr (JMPZ (negate 13)) 
>   $ EmptyInstr