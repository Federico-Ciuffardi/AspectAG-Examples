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

> module MicroPascalPrettyPrint where

> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import MicroPascalSyntax
> import ExprSyntax
> import ExprPrettyPrint

> newline iLvl = "\n" ++ (concat.(take iLvl).repeat) "  "

> precedenceP_asp
>   =  inh precedence p_Assign ch_assignExpr (return 0)
>  .+: inh precedence p_If ch_ifCond (return 0)
>  .+: inh precedence p_While ch_whileCond (return 0)
>  .+: inh precedence p_WriteLn ch_writeLnExpr (return 0)
>  .+: pPrintE_asp

> $(attLabels [("indentLvl", ''Int)])

> ident_asp
>   =  inh indentLvl p_Program ch_programBody (return 1)

  .+: (inh indentLvl p_Stmt ch_tailStmtList $
      do lhsInd <- at lhs indentLvl
         (return lhsInd+1))

>  .+: inh indentLvl p_Stmt ch_headStmt (at lhs indentLvl)
>  .+: inh indentLvl p_Stmt ch_tailStmtList (at lhs indentLvl)
>  .+: (inh indentLvl p_If ch_ifThen $
>      do lhsInd <- at lhs indentLvl
>         return (lhsInd+1))
>  .+: (inh indentLvl p_If ch_ifElse $
>      do lhsInd <- at lhs indentLvl
>         return (lhsInd+1))
>  .+: (inh indentLvl p_While ch_whileDo $
>      do lhsInd <- at lhs indentLvl
>         return (lhsInd+1))
>  .+: precedenceP_asp

> pPrintP_asp
>   =  (syn pPrint p_Program $
>      do name     <- ter ch_name
>         defPP    <- at ch_programDefs pPrint
>         bodPP    <- at ch_programBody pPrint
>         return ("program " ++ name ++ ";\n" ++ 
>                 "var" ++ defPP ++ "\n" ++ 
>                 "begin" ++ 
>                    bodPP ++ "\n" ++
>                  "end."))

>  .+: syn pPrint p_EmptyDef (return "")
>  .+: (syn pPrint p_Def $
>      do varName       <- ter ch_varName
>         varType       <- ter ch_varType
>         tailDefListPP <- at ch_tailDefList pPrint
>         let after = if null tailDefListPP then ";" else ";\n   "
>         return (" " ++ varName ++ " : " ++ show varType ++ after ++ tailDefListPP))

>  .+: syn pPrint p_EmptyStmt (return "")

>  .+: (syn pPrint p_Stmt $
>      do headStmt       <- at ch_headStmt pPrint
>         tailStmtListPP <- at ch_tailStmtList pPrint
>         lhsInd         <- at lhs indentLvl
>         let after = if null tailStmtListPP then "" else ";"
>         return (newline lhsInd ++ headStmt ++ after ++ tailStmtListPP))

>  .+: (syn pPrint p_Assign $
>      do varName <- ter ch_assignName
>         expr    <- at ch_assignExpr pPrint
>         return (varName ++ " := " ++ expr))

>  .+: (syn pPrint p_If $
>      do exprPP <- at ch_ifCond pPrint
>         thenPP <- at ch_ifThen pPrint
>         elsePP <- at ch_ifElse pPrint
>         lhsInd <- at lhs indentLvl
>         return ("if " ++ exprPP ++ " then" ++ newline lhsInd ++
>                 "begin" ++
>                   thenPP ++ newline lhsInd ++
>                 "end" ++ newline lhsInd ++
>                 "else" ++ newline lhsInd ++
>                 "begin" ++ 
>                   elsePP ++ newline lhsInd ++
>                 "end"))

>  .+: (syn pPrint p_While $
>      do exprPP <- at ch_whileCond pPrint
>         doPP   <- at ch_whileDo pPrint
>         lhsInd <- at lhs indentLvl
>         return ("while " ++ exprPP ++ " do" ++ newline lhsInd ++
>                 "begin" ++
>                   doPP ++ newline lhsInd ++
>                 "end"))


>  .+: (syn pPrint p_WriteLn $
>      do expPP <- at ch_writeLnExpr pPrint
>         return ("writeln(" ++ expPP ++ ")"))

                                
>  .+: (syn pPrint p_ReadLn $
>      do varName  <- ter ch_readLnName
>         return ("readln(" ++ varName ++ ")"))

>  .+: ident_asp


> pPrintProg e = sem_Program pPrintP_asp e emptyAtt #. pPrint

> test1 = putStrLn $ pPrintProg 
>         (Program "ejemplo1" 
>         EmptyDef
>         EmptyStmt)

program ejemplo1;
var
begin
end.


> test2 = putStrLn $ pPrintProg 
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


> test3 = putStrLn $ pPrintProg 
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


> -- type errors on purpose
> test4 = putStrLn $ pPrintProg 
>         (Program "ejemplo7" 
>         (Def "b" TBool 
>         (Def "x" TInt 
>         EmptyDef))
>         (Stmt (ReadLn "b")
>         (Stmt (ReadLn "x")
>         (Stmt (If (Bop (Var "b") And (Var "x")) 
>           (Stmt (WriteLn (Var "x"))
>           EmptyStmt) 
>           (Stmt (While (Bop (Var "x") Mul (Val $ VInt 4))
>             (Stmt (WriteLn (Var "b"))
>             EmptyStmt)) 
>           EmptyStmt))
>         (Stmt (Assign "x" (Bop (Bop (Var "x") Add (Val $ VBool True)) And (Bop (Var "b") Or (Val $ VInt 8)) ))
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

> test5 = putStrLn $ pPrintProg 
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
