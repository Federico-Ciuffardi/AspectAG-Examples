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
> import Data.Maybe
> import MachineLangSyntax
> import Criterion.Main


atts defs

> type Stack  = [Integer]
> type VarEnv = [(String, Integer)]
> type State  = (Stack,VarEnv)
> type Code   = [Instr]

aux funcs for state

> updateState inst (stack,env) = 
>   do i <- processIO inst stack
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
> updateStack (JUMP i) stack  _      = stack
> updateStack (JMPZ i) (h:tail) _    = tail
> updateStack (LOAD  var) stack env  = (fromJust $ lookup var env):stack
> updateStack (STORE var) (h:tail) _ = tail
> updateStack READ stack _           = stack
> updateStack WRITE (h:tail) _       = tail
> updateStack SKIP stack _           = stack
> updateStack a b c = error ("updateStack error: " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " ")

aux funcs for jumps

> nextInstr (JMPZ i) (stackHead:_,_) prvInstrs nxtInstrs = 
>  if stackHead /= 0 then (prvInstrs'', (arrToInstrs nxtInstrs'')) else (prvInstrs', (arrToInstrs nxtInstrs'))
>   where (prvInstrs', nxtInstrs')   = jump i prvInstrs (instrsToArr nxtInstrs)
>         (prvInstrs'', nxtInstrs'') = jump 1 prvInstrs (instrsToArr nxtInstrs)
> nextInstr (JUMP i) _  prvInstrs nxtInstrs = (prvInstrs', (arrToInstrs nxtInstrs'))
>   where (prvInstrs', nxtInstrs')   = jump i prvInstrs (instrsToArr nxtInstrs)
> nextInstr _        _  prvInstrs nxtInstrs = (prvInstrs', (arrToInstrs nxtInstrs'))
>   where (prvInstrs', nxtInstrs')   = jump 1 prvInstrs (instrsToArr nxtInstrs)

> jump 0 prvInstrs nxtInstrs         = (prvInstrs, nxtInstrs) 
> jump i (pHead:pTail) (nHead:nTail) | i > 0 = jump (i-1) (nHead:pHead:pTail) (nTail) 
>                                    | i < 0 = jump (i+1) (pTail) (pHead:nHead:nTail) 
> jump i (pHead:pTail) []            | i > 0 = (pHead:pTail, [])
>                                    | i < 0 = jump (i+1) pTail [pHead] 
> jump i [] (nHead:nTail)            | i > 0 = jump (i-1) [nHead] nTail
>                                    | i < 0 = ([], nHead:nTail)

aux funcs for env

> updateEnv (STORE var) (val:_) []                 = [(var,val)]
> updateEnv (STORE var) (val:_) ((var',val'):tail) | var == var' = (var,val):tail
>                                                  | otherwise   = (var',val') : updateEnv (STORE var) [val] tail
> updateEnv _ _ env = env

 getState stt prvInstrs nxtInstrs = sem_Instrs state_asp nxtInstrs ((state =. stt ) *. (prevInstrs =. prvInstrs) *. emptyAtt) #. state
 interpret prog = getState (return ([],[])) [] prog 

> interp :: [Instr] -> Instrs -> State -> IO State
> interp prevInstrs (Instr headInstr tailInstrs) state = 
>   do state' <- (updateState headInstr state)
>      let (prevInstrs', nextInstrs') = nextInstr headInstr state prevInstrs (Instr headInstr tailInstrs)
>      interp prevInstrs' nextInstrs' state'
> interp _ EmptyInstr state = return state

> interpret prog = interp [] prog ([],[])

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

> main = defaultMain [
>   bgroup "Interpreter"
>     [ bench "test0" $ nfIO test0
>     , bench "test1" $ nfIO test1_1
>     , bench "test2" $ nfIO test2
>     , bench "test3" $ nfIO test3_1
>     , bench "test4" $ nfIO test4
>     , bench "test5" $ nfIO test5
>     , bench "test6" $ nfIO test6
>     , bench "test7" $ nfIO test7
>     , bench "test8" $ nfIO test8
>     , bench "test9" $ nfIO test9_1
>     , bench "test10" $ nfIO test10
>     , bench "test14" $ nfIO test14
>     , bench "test15" $ nfIO test15
>     , bench "test16" $ nfIO test16
>     , bench "test17" $ nfIO test17_2
>     ]
>   ]