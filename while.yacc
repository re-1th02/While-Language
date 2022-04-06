open DataTypes

exception ERROR of string;

                           
structure IntVariables :
    sig val findI : string -> (string) option
        val addI : (string) -> unit
        val nullifyI : unit -> unit
        end = struct
            val TableSize = 422 (* 211 *)
            val HashFactor = 5
            val hash = fn s  => List.foldr (fn (c,v) => (v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
            val HashTable = Array.array(TableSize,nil) : (string) list Array.array
            val addI = fn (s) => let val i = hash s in Array.update(HashTable,i,(s) :: (Array.sub(HashTable, i))) end
            val findI = fn s => let  val i = hash s
                                    fun f ((key)::r) = if s=key then SOME key else f r | f nil = NONE
                                in f (Array.sub(HashTable, i)) end
            val nullifyI     = fn ()=> let fun f 422 = () | f i = let val _ = Array.update(HashTable, i, nil) in (f (i+1)) end in (f 0) end
end;

structure BoolVariables :
    sig val findB : string -> (string) option
        val addB : (string) -> unit
        val nullifyB : unit -> unit
        end = struct
            val TableSize = 422 (* 211 *)
            val HashFactor = 5
            val hash = fn s  => List.foldr (fn (c,v) => (v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
            val HashTable = Array.array(TableSize,nil) : (string) list Array.array
            val addB = fn (s) => let val i = hash s in Array.update(HashTable,i,(s) :: (Array.sub(HashTable, i))) end
            val findB = fn s => let  val i = hash s
                                    fun f ((key)::r) = if s=key then SOME key else f r | f nil = NONE
                                in f (Array.sub(HashTable, i)) end
            val nullifyB     = fn ()=> let fun f 422 = () | f i = let val _ = Array.update(HashTable, i, nil) in (f (i+1)) end in (f 0) end
end;
fun badCh (bad, line, col, s) = let val _ = print("ERROR!!!  : Variable '"^bad ^  "' has never declared as "^s^". Line: "  ^ Int.toString(line)^" , col : "^Int.toString(col)^"\n") in OS.Process.exit(OS.Process.success) end;

open IntVariables;
open BoolVariables;

%%
%name WHILE
%term CARET | DVBAR | EOF | EQUALS
| IDE of string
| INT
| ILLCH | INPUT | LPAR | NEW | PRO | VAR | RPAR
| LFLO | RFLO | PLUS | MINUS | TIMES | DIV | SEMICOLON | COLON | DOUBLECOLON | COMMA | NUMBER of int
| LT | GT | LEQ | GEQ | NEQ | EQ | AND | TT | FF | NOT | READ | WRITE | IF | THEN | ELSE | ENDIF | WHI | DO | ENDWH | BOOL | NEG | MOD

%nonterm begin of Program
| Block of Block
| DeclarationSeq of DeclarationSeq
| parallelDecSeq of (string list * Type) list
| Declaration of string list * Type
| VariableList of string list
| parallelVarList of string list
| Variable of Variable
| Var of string
| CommandList of CommandList
| parallelCmdList of Command list
| parallelCmdList1 of Command list
| Command of Command
| IntExpression of IntExpression
| NoAddExpression of NoAddExpression
| NoMultiExpression of NoMultiExpression
| BoolExpression of BoolExpression
| Boolterm of Boolterm
| BoolFactor of BoolFactor
| Comparison of Comparison
| RelOP of Relop
| Type of Type
| AddOP of OP
| MultiOP of OP

%pos int
%eop EOF
%noshift EOF
%nonassoc DVBAR EOF EQUALS ILLCH SEMICOLON COLON DOUBLECOLON LPAR VAR PRO RPAR LFLO RFLO COMMA NUMBER LT GT LEQ GEQ EQ NEQ NOT AND NEG MOD
%nodefault
%keyword VAR PRO INT BOOL READ WRITE IF ELSE THEN WHI DO ENDWH ENDIF TT FF
%arg (fileName) : string
%verbose
%%

begin   : PRO IDE DOUBLECOLON Block ((let val _ = nullifyI() and _ = nullifyB() in PROG (IDE, Block) end))
Block   : DeclarationSeq CommandList ((BLK_DEC (DeclarationSeq, CommandList))) 
        | CommandList ((BLK_NODEC CommandList))
DeclarationSeq      : parallelDecSeq ((DEC parallelDecSeq))
parallelDecSeq      : Declaration (([Declaration])) 
                    | Declaration parallelDecSeq ((Declaration::parallelDecSeq)) 
Declaration         : VAR VariableList COLON INT SEMICOLON ((let val _ = List.app addI VariableList in (VariableList, INT) end)) 
                    |  VAR VariableList COLON BOOL SEMICOLON ((let val _ = List.app addB VariableList in (VariableList, BOOL) end))
VariableList        : parallelVarList ((parallelVarList))
parallelVarList     : IDE (([IDE]))| IDE COMMA parallelVarList ((IDE::parallelVarList))
CommandList         : LFLO parallelCmdList RFLO((SEQ parallelCmdList)) 
                    | LFLO RFLO ((SEQ []))
parallelCmdList     : Command SEMICOLON (([Command])) 
                    | Command SEMICOLON parallelCmdList((Command :: parallelCmdList))
Variable            : Var ((if findB Var = NONE andalso findI Var = NONE then let val _ = badCh (Var, Varleft, Varright, "any type") in (Int Var) end else if findB Var = NONE then (Int Var) else (Bool Var) ))
Var                 : LPAR Var RPAR ((Var)) | IDE ((let val _ = PositionTable.add(IDE, (IDEleft, IDEright)) in IDE end))
Command             : IDE EQUALS Var ((if findB IDE = NONE andalso findI IDE = NONE then let val _ = badCh (IDE, IDEleft, IDEright, "any type") in SET_INT (Int IDE, (IEXP_sub (IEXP1_sub (IVAL (Int Var))))) end else if findB IDE = NONE then let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in SET_INT (Int IDE, (IEXP_sub (IEXP1_sub (IVAL (Int Var))))) end else let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in SET_BOOL (Bool IDE, (NO_OR (NO_AND (BFACT (BVAL (Bool Var)))))) end))
                    | IDE EQUALS IntExpression ((let val _ = if findI IDE = NONE then badCh (IDE, IDEleft, IDEright, "int") else () in SET_INT (Int IDE, IntExpression) end)) 
                    | IDE EQUALS BoolExpression ((let val _ = if findB IDE = NONE then badCh (IDE, IDEleft, IDEright, "bool") else () in SET_BOOL (Bool IDE, BoolExpression) end)) 
                    | READ Variable ((READ Variable)) 
                    | WRITE IntExpression ((WRITEI IntExpression))
                    | WRITE Var ((if findB Var = NONE andalso findI Var = NONE then let val _ = badCh (Var, Varleft, Varright, "any type") in (WRITEI (IEXP_sub (IEXP1_sub (IVAL (Int Var))))) end else if findB Var = NONE then let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in (WRITEI (IEXP_sub (IEXP1_sub (IVAL (Int Var))))) end else let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in (WRITEB (NO_OR (NO_AND (BFACT (BVAL (Bool Var)))))) end))
                    | WRITE BoolExpression ((WRITEB BoolExpression)) 
                    | IF BoolExpression THEN CommandList ELSE CommandList ENDIF ((ITE (BoolExpression, CommandList1, CommandList2))) 
                    | IF Variable THEN CommandList ELSE CommandList ENDIF ((ITE ((NO_OR (NO_AND (BFACT (BVAL Variable)))), CommandList1, CommandList2)))
                    | WHI BoolExpression DO CommandList ENDWH ((WH (BoolExpression, CommandList)))
                    | WHI Variable DO CommandList ENDWH ((WH ((NO_OR (NO_AND (BFACT (BVAL Variable)))), CommandList)))
IntExpression       : IntExpression AddOP NoAddExpression   ((IEXP (IntExpression,                          AddOP, NoAddExpression)))
                    | IntExpression AddOP Var               ((let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in IEXP (IntExpression,         AddOP, (IEXP1_sub (IVAL (Int Var)))) end))
                    | Var AddOP NoAddExpression             ((let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in IEXP ((IEXP_sub (IEXP1_sub (IVAL (Int Var)))),AddOP, NoAddExpression) end))
                    | Var AddOP Var                         ((let val _ = if findI Var1 = NONE then badCh (Var1, Var1left, Var1right, "int") else if findI Var2 = NONE then badCh (Var2, Var2left, Var2right, "int") else () in IEXP ((IEXP_sub (IEXP1_sub (IVAL (Int Var1)))),AddOP,(IEXP1_sub (IVAL (Int Var2)))) end))
                    | NoAddExpression ((IEXP_sub NoAddExpression))
NoAddExpression     : NoAddExpression MultiOP NoMultiExpression ((IEXP1 (NoAddExpression,               MultiOP, NoMultiExpression))) 
                    | NoAddExpression MultiOP Var               ((let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in IEXP1 (NoAddExpression,               MultiOP, (IVAL (Int Var))) end))
                    | Var MultiOP NoMultiExpression             ((let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in IEXP1 (IEXP1_sub (IVAL (Int Var)),    MultiOP, NoMultiExpression) end))
                    | Var MultiOP Var                           ((let val _ = if findI Var1 = NONE then badCh (Var1, Var1left, Var1right, "int") else if findI Var2 = NONE then badCh (Var2, Var2left, Var2right, "int") else () in IEXP1 (IEXP1_sub (IVAL (Int Var1)),    MultiOP, (IVAL (Int Var2))) end))
                    | NoMultiExpression ((IEXP1_sub NoMultiExpression))
NoMultiExpression   : NUMBER ((NUM NUMBER))
                    | PLUS NUMBER ((NUM NUMBER))
                    | LPAR IntExpression RPAR ((IBRAC IntExpression)) 
                    | NEG NoMultiExpression ((NEG NoMultiExpression))
                    | NEG Var ((let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in NEG ((IVAL (Int Var))) end))
AddOP               : PLUS ((PLUS)) 
                    | MINUS ((MINUS)) 
MultiOP             : TIMES ((TIMES)) 
                    | DIV ((DIV)) 
                    | MOD ((MOD))
BoolExpression      : BoolExpression DVBAR Boolterm ((OR (BoolExpression, Boolterm))) 
                    | BoolExpression DVBAR Var      ((let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in OR (BoolExpression, (NO_AND (BFACT (BVAL (Bool Var))))) end))
                    | Var DVBAR Boolterm            ((let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in OR (NO_OR (NO_AND (BFACT (BVAL (Bool Var)))), Boolterm) end))
                    | Var DVBAR Var                 ((let val _ = if findB Var1 = NONE then badCh (Var1, Var1left, Var1right, "bool") else if findB Var2 = NONE then badCh (Var2, Var2left, Var2right, "bool") else () in OR (NO_OR (NO_AND (BFACT (BVAL (Bool Var1)))), (NO_AND (BFACT (BVAL (Bool Var2))))) end))
                    | Boolterm ((NO_OR Boolterm))
Boolterm            : Boolterm AND BoolFactor ((AND (Boolterm, BoolFactor))) 
                    | Boolterm AND Var ((let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in AND (Boolterm, (BVAL (Bool Var))) end))
                    | Var AND BoolFactor ((let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in AND ((NO_AND (BFACT (BVAL (Bool Var)))), BoolFactor) end))
                    | Var AND Var           ((let val _ = if findB Var1 = NONE then badCh (Var1, Var1left, Var1right, "bool") else if findB Var2 = NONE then badCh (Var2, Var2left, Var2right, "bool") else () in AND ((NO_AND (BFACT (BVAL (Bool Var1)))), (BVAL (Bool Var2))) end))
                    | Comparison ((NO_AND Comparison))
BoolFactor          : TT ((TT)) | FF ((FF))
                    | LPAR BoolExpression RPAR ((BBRAC BoolExpression)) 
                    | NOT Comparison ((NOT Comparison))
                    | NOT Var ((let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in NOT (BFACT (BVAL (Bool Var))) end))
Comparison          : IntExpression RelOP IntExpression ((ICOMP (IntExpression1, RelOP, IntExpression2))) 
                    | Var RelOP IntExpression ((let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in ICOMP ((IEXP_sub (IEXP1_sub (IVAL (Int Var)))), RelOP, IntExpression) end))
                    | IntExpression RelOP Var ((let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in ICOMP (IntExpression, RelOP, (IEXP_sub (IEXP1_sub (IVAL (Int Var))))) end))
                    | Var RelOP Var ((if findB Var1 = NONE andalso findI Var1 = NONE then let val _ = badCh (Var1, Var1left, Var1right, "any type") in (ICOMP ((IEXP_sub (IEXP1_sub (IVAL (Int Var1)))), RelOP, (IEXP_sub (IEXP1_sub (IVAL (Int Var2)))))) end else if findB Var1 = NONE then let val _ = if findI Var2 = NONE then badCh (Var2, Var2left, Var2right, "int") else () in (ICOMP ((IEXP_sub (IEXP1_sub (IVAL (Int Var1)))), RelOP, (IEXP_sub (IEXP1_sub (IVAL (Int Var2)))))) end else let val _ = if findB Var2 = NONE then badCh (Var2, Var2left, Var2right, "bool") else () in  ((BCOMP ((BFACT (BVAL (Bool Var1))), RelOP, (BVAL (Bool Var2))))) end))
                    | Comparison RelOP BoolFactor ((BCOMP (Comparison, RelOP, BoolFactor)))
                    | Comparison RelOP Var ((let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in BCOMP (Comparison, RelOP, (BVAL (Bool Var))) end))
                    | Var RelOP BoolFactor ((let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in BCOMP ((BFACT (BVAL (Bool Var))), RelOP, BoolFactor) end))
                    | BoolFactor ((BFACT BoolFactor))
RelOP               : LT ((LT)) 
                    | GT ((GT)) 
                    | LEQ ((LEQ)) 
                    | GEQ ((GEQ)) 
                    | NEQ ((NEQ)) 
                    | EQ ((EQ))