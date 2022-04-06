signature WHILE_TOKENS =
sig
type ('a,'b) token
type svalue
val MOD:  'a * 'a -> (svalue,'a) token
val NEG:  'a * 'a -> (svalue,'a) token
val BOOL:  'a * 'a -> (svalue,'a) token
val ENDWH:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHI:  'a * 'a -> (svalue,'a) token
val ENDIF:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val WRITE:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val FF:  'a * 'a -> (svalue,'a) token
val TT:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val NEQ:  'a * 'a -> (svalue,'a) token
val GEQ:  'a * 'a -> (svalue,'a) token
val LEQ:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val NUMBER: (int) *  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val DOUBLECOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val RFLO:  'a * 'a -> (svalue,'a) token
val LFLO:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val PRO:  'a * 'a -> (svalue,'a) token
val NEW:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val INPUT:  'a * 'a -> (svalue,'a) token
val ILLCH:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val IDE: (string) *  'a * 'a -> (svalue,'a) token
val EQUALS:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val DVBAR:  'a * 'a -> (svalue,'a) token
val CARET:  'a * 'a -> (svalue,'a) token
end
signature WHILE_LRVALS=
sig
structure Tokens : WHILE_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
