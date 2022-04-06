signature DATATYPES =
sig datatype 
Program = PROG of string * Block and
Block   = BLK_DEC  of DeclarationSeq * CommandList | BLK_NODEC of CommandList and
DeclarationSeq = DEC of (string list * Type) list and
CommandList = SEQ of Command list and
Command = SET_INT of Variable * IntExpression | SET_BOOL of Variable * BoolExpression | READ of Variable | WRITEI of IntExpression | WRITEB of BoolExpression | ITE of BoolExpression * CommandList * CommandList | WH of BoolExpression * CommandList and
Variable = Int of string | Bool of string and 
IntExpression = IEXP of IntExpression * OP * NoAddExpression| IEXP_sub of NoAddExpression  and
NoAddExpression = IEXP1 of NoAddExpression * OP * NoMultiExpression | IEXP1_sub of NoMultiExpression and
NoMultiExpression = NUM of int | IVAL of Variable | IBRAC of IntExpression | NEG of NoMultiExpression  and
BoolExpression = OR of BoolExpression * Boolterm | NO_OR of Boolterm and
Boolterm = AND of Boolterm * BoolFactor | NO_AND of Comparison and
BoolFactor = TT | FF | BVAL of Variable | BBRAC of BoolExpression | NOT of Comparison and
Comparison = ICOMP of IntExpression* Relop* IntExpression | BCOMP of Comparison * Relop * BoolFactor | BFACT of BoolFactor and
Relop = LT | GT | LEQ | EQ | GEQ | NEQ and
OP = PLUS | MINUS | TIMES | DIV | MOD and
Type = INT | BOOL and
UniversalDT =  cmdlist of Command list | cmd of Command | var of Variable | intexp of IntExpression | noaddexp of NoAddExpression | nomultexp of NoMultiExpression 
                    |   boolexp of BoolExpression | bterm of Boolterm | bfact of BoolFactor | cmp  of Comparison | num of int | str of string | SET | seq | ite | wh | read | WRITE | 
                    oop of string * OP | reloop of string * Relop | or | nd | not
end;

structure DataTypes : DATATYPES =
struct datatype
Program = PROG of string * Block and
Block   = BLK_DEC  of DeclarationSeq * CommandList | BLK_NODEC of CommandList and
DeclarationSeq = DEC of (string list * Type) list and
CommandList = SEQ of Command list and
Command = SET_INT of Variable * IntExpression | SET_BOOL of Variable * BoolExpression | READ of Variable | WRITEI of IntExpression | WRITEB of BoolExpression | ITE of BoolExpression * CommandList * CommandList | WH of BoolExpression * CommandList and
Variable = Int of string | Bool of string and 
IntExpression = IEXP of IntExpression * OP * NoAddExpression| IEXP_sub of NoAddExpression  and
NoAddExpression = IEXP1 of NoAddExpression * OP * NoMultiExpression | IEXP1_sub of NoMultiExpression and
NoMultiExpression = NUM of int | IVAL of Variable | IBRAC of IntExpression | NEG of NoMultiExpression  and
BoolExpression = OR of BoolExpression * Boolterm | NO_OR of Boolterm and
Boolterm = AND of Boolterm * BoolFactor | NO_AND of Comparison and
BoolFactor = TT | FF | BVAL of Variable | BBRAC of BoolExpression | NOT of Comparison and
Comparison = ICOMP of IntExpression* Relop* IntExpression | BCOMP of Comparison * Relop * BoolFactor | BFACT of BoolFactor and
Relop = LT | GT | LEQ | EQ | GEQ | NEQ and
OP = PLUS | MINUS | TIMES | DIV | MOD and
Type = INT | BOOL and
UniversalDT =  cmdlist of Command list |cmd of Command | var of Variable | intexp of IntExpression | noaddexp of NoAddExpression | nomultexp of NoMultiExpression 
                    |   boolexp of BoolExpression | bterm of Boolterm | bfact of BoolFactor | cmp  of Comparison | num of int | str of string | SET | seq | ite | wh | read | WRITE | 
                    oop of string * OP | reloop of string * Relop | or | nd | not
end;