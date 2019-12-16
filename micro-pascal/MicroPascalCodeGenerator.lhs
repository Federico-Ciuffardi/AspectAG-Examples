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

> module MicroPascalCodeGenerator where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import MicroPascalSyntax
> import ExprSyntax
> import ExprCodeGenerator
> import MachineLangSyntax

> generateP_asp
>   =  (syn generate p_Program $
>      do bodIns <- at ch_programBody generate
>         return bodIns)

>  .+: syn generate p_EmptyDef (return EmptyInstr)
>  .+: syn generate p_Def (return EmptyInstr)

>  .+: syn generate p_EmptyStmt (return EmptyInstr)

>  .+: (syn generate p_Stmt $
>      do headIns         <- at ch_headStmt generate
>         tailStmtListIns <- at ch_tailStmtList generate
>         return $ concatInstrs tailStmtListIns headIns)

>  .+: (syn generate p_Assign $
>      do varName <- ter ch_assignName
>         exprIns <- at ch_assignExpr generate
>         return $ Instr (STORE varName)
>                $ exprIns)

>  .+: (syn generate p_If $
>      do exprIns <- at ch_ifCond generate
>         thenIns <- at ch_ifThen generate
>         elseIns <- at ch_ifElse generate
>         return $ concatInstrs elseIns
>                $ Instr (JUMP $ instrCount elseIns + 1)
>                $ concatInstrs thenIns
>                $ Instr (JMPZ $ instrCount thenIns + 2)
>                $ exprIns)

>  .+: (syn generate p_While $
>      do exprIns <- at ch_whileCond generate
>         doIns   <- at ch_whileDo generate
>         return $ Instr (JUMP $ negate (instrCount doIns + instrCount exprIns + 2) )
>                $ concatInstrs doIns
>                $ Instr (JMPZ $ instrCount doIns + 2)
>                $ exprIns)


>  .+: (syn generate p_WriteLn $
>      do exprIns <- at ch_writeLnExpr generate
>         return $ Instr WRITE
>                $ exprIns)

                                
>  .+: (syn generate p_ReadLn $
>      do varName  <- ter ch_readLnName
>         return $ Instr (STORE varName)
>                $ Instr READ
>                $ EmptyInstr)

>  .+: generateE_asp


> generateProgram e = sem_Program generateP_asp e emptyAtt #. generate

> test1 = generateProgram $
>         (Program "ejemplo1" 
>         EmptyDef
>         EmptyStmt)

program ejemplo1;
var
begin
end.


> test2 = generateProgram  $
>         (Program "ejemplo2" 
>         (Def "x" TInt 
>         (Def "y" TInt 
>         (Def "b" TBool 
>         EmptyDef)))
>         (Stmt (Assign "x" (Val $ VInt 10))
>         (Stmt (Assign "y" (Bop (Var "x") Mul (Bop (Val $ VInt 3) Add (Val $ VInt 2))))
>         (Stmt (Assign "b" (Val $ VBool True))
>         (Stmt (Assign "b" (Uop Not (Bop (Var "x") Lt (Val $ VInt 10))))
>         EmptyStmt)))))

program ejemplo2;
var x : integer;
    y : integer;
    b : boolean;
begin
  x := 10;
  y := x * (3 + 2);
  b := true;
  b := not (x < 10)
end.


> test3 = generateProgram $
>         (Program "ejemplo3" 
>         (Def "w" TInt 
>         (Def "x" TBool 
>         EmptyDef))
>         (Stmt (ReadLn "w")  
>         (Stmt (Assign "x" (Bop (Var "w") Lt (Val $ VInt 10)))
>         (Stmt (If (Var "x") 
>           (Stmt (WriteLn (Var "w"))
>           EmptyStmt) 
>           (Stmt (WriteLn (Val $ VInt 10)) 
>           EmptyStmt))
>         EmptyStmt))))

program ejemplo3;
var w : integer;
    x : boolean;
begin
  readln(w);
  x := w < 10;
  if x then
  begin
    writeln(w)
  end
  else
  begin
    writeln(10)
  end
end.

> test4 = generateProgram $
>         (Program "ejemplo7.1" 
>         (Def "b" TInt 
>         (Def "x" TInt 
>         EmptyDef))
>         (Stmt (ReadLn "b")
>         (Stmt (ReadLn "x")
>         (Stmt (If (Bop (Var "b") Equ (Var "x")) 
>           (Stmt (WriteLn (Var "x"))
>           EmptyStmt) 
>           (Stmt (While (Bop (Var "x") Lt (Val $ VInt 4))
>             (Stmt (WriteLn (Var "b"))
>             EmptyStmt)) 
>           EmptyStmt))
>         (Stmt (Assign "x" (Bop (Bop (Var "x") Add (Val $ VBool True)) Mul (Bop (Var "b") Mul (Val $ VInt 8)) ))
>         EmptyStmt)))))

program ejemplo7;
var b : boolean;
    x : integer;
begin
  readln(b);
  readln(x);
  if b and x then
  begin
    writeln(x)
  end
  else
  begin
    while x * 4 do
    begin
      writeln(b)
    end
  end;
  x := (x + true) and (b or 10)
end.

> test5 = generateProgram $
>         (Program "test5" 
>         (Def "x" TInt 
>         (Def "y" TInt 
>         EmptyDef))
>         (Stmt (If (Val $ VBool False) 
>           (Stmt (Assign "x" (Val $ VInt 0)) 
>           EmptyStmt) 
>           (Stmt (ReadLn "x") 
>           EmptyStmt))
>         (Stmt (WriteLn (Var "x"))  
>         EmptyStmt)))

program test5;
var x : integer;
    y : integer;
begin
  if false then
  begin
    x := 0
  end
  else
  begin
    readln(x)
  end;
  writeln(x)
end.
