structure WHILELrVals= WHILELrValsFun(structure Token   = LrParser.Token);
structure WHILELex   = WHILELexFun   (structure Tokens  = WHILELrVals.Tokens);
structure WHILEParser= JoinWithArg(structure ParserData = WHILELrVals.ParserData
                                structure Lex=WHILELex
                                structure LrParser=LrParser);


(* fun comp file= WHILE.compile file; *)

