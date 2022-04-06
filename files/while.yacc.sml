functor WHILELrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : WHILE_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
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


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\159\000\013\000\159\000\016\000\159\000\017\000\159\000\
\\018\000\159\000\019\000\159\000\020\000\159\000\025\000\159\000\
\\026\000\159\000\027\000\159\000\028\000\159\000\029\000\159\000\
\\030\000\159\000\031\000\159\000\038\000\159\000\042\000\159\000\
\\046\000\159\000\000\000\
\\001\000\002\000\160\000\013\000\160\000\016\000\160\000\017\000\160\000\
\\018\000\160\000\019\000\160\000\020\000\160\000\025\000\160\000\
\\026\000\160\000\027\000\160\000\028\000\160\000\029\000\160\000\
\\030\000\160\000\031\000\160\000\038\000\160\000\042\000\160\000\
\\046\000\160\000\000\000\
\\001\000\002\000\172\000\013\000\172\000\016\000\172\000\017\000\172\000\
\\018\000\070\000\019\000\069\000\020\000\172\000\025\000\172\000\
\\026\000\172\000\027\000\172\000\028\000\172\000\029\000\172\000\
\\030\000\172\000\031\000\172\000\038\000\172\000\042\000\172\000\
\\046\000\068\000\000\000\
\\001\000\002\000\173\000\013\000\173\000\016\000\173\000\017\000\173\000\
\\018\000\070\000\019\000\069\000\020\000\173\000\025\000\173\000\
\\026\000\173\000\027\000\173\000\028\000\173\000\029\000\173\000\
\\030\000\173\000\031\000\173\000\038\000\173\000\042\000\173\000\
\\046\000\068\000\000\000\
\\001\000\002\000\174\000\013\000\174\000\016\000\174\000\017\000\174\000\
\\018\000\070\000\019\000\069\000\020\000\174\000\025\000\174\000\
\\026\000\174\000\027\000\174\000\028\000\174\000\029\000\174\000\
\\030\000\174\000\031\000\174\000\038\000\174\000\042\000\174\000\
\\046\000\068\000\000\000\
\\001\000\002\000\175\000\013\000\175\000\016\000\175\000\017\000\175\000\
\\018\000\070\000\019\000\069\000\020\000\175\000\025\000\175\000\
\\026\000\175\000\027\000\175\000\028\000\175\000\029\000\175\000\
\\030\000\175\000\031\000\175\000\038\000\175\000\042\000\175\000\
\\046\000\068\000\000\000\
\\001\000\002\000\176\000\013\000\176\000\016\000\176\000\017\000\176\000\
\\018\000\070\000\019\000\069\000\020\000\176\000\025\000\176\000\
\\026\000\176\000\027\000\176\000\028\000\176\000\029\000\176\000\
\\030\000\176\000\031\000\176\000\038\000\176\000\042\000\176\000\
\\046\000\068\000\000\000\
\\001\000\002\000\177\000\013\000\177\000\016\000\177\000\017\000\177\000\
\\018\000\177\000\019\000\177\000\020\000\177\000\025\000\177\000\
\\026\000\177\000\027\000\177\000\028\000\177\000\029\000\177\000\
\\030\000\177\000\031\000\177\000\038\000\177\000\042\000\177\000\
\\046\000\177\000\000\000\
\\001\000\002\000\178\000\013\000\178\000\016\000\178\000\017\000\178\000\
\\018\000\178\000\019\000\178\000\020\000\178\000\025\000\178\000\
\\026\000\178\000\027\000\178\000\028\000\178\000\029\000\178\000\
\\030\000\178\000\031\000\178\000\038\000\178\000\042\000\178\000\
\\046\000\178\000\000\000\
\\001\000\002\000\179\000\013\000\179\000\016\000\179\000\017\000\179\000\
\\018\000\179\000\019\000\179\000\020\000\179\000\025\000\179\000\
\\026\000\179\000\027\000\179\000\028\000\179\000\029\000\179\000\
\\030\000\179\000\031\000\179\000\038\000\179\000\042\000\179\000\
\\046\000\179\000\000\000\
\\001\000\002\000\180\000\013\000\180\000\016\000\180\000\017\000\180\000\
\\018\000\180\000\019\000\180\000\020\000\180\000\025\000\180\000\
\\026\000\180\000\027\000\180\000\028\000\180\000\029\000\180\000\
\\030\000\180\000\031\000\180\000\038\000\180\000\042\000\180\000\
\\046\000\180\000\000\000\
\\001\000\002\000\181\000\013\000\181\000\016\000\181\000\017\000\181\000\
\\018\000\181\000\019\000\181\000\020\000\181\000\025\000\181\000\
\\026\000\181\000\027\000\181\000\028\000\181\000\029\000\181\000\
\\030\000\181\000\031\000\181\000\038\000\181\000\042\000\181\000\
\\046\000\181\000\000\000\
\\001\000\002\000\182\000\013\000\182\000\016\000\182\000\017\000\182\000\
\\018\000\182\000\019\000\182\000\020\000\182\000\025\000\182\000\
\\026\000\182\000\027\000\182\000\028\000\182\000\029\000\182\000\
\\030\000\182\000\031\000\182\000\038\000\182\000\042\000\182\000\
\\046\000\182\000\000\000\
\\001\000\002\000\183\000\013\000\183\000\016\000\183\000\017\000\183\000\
\\018\000\183\000\019\000\183\000\020\000\183\000\025\000\183\000\
\\026\000\183\000\027\000\183\000\028\000\183\000\029\000\183\000\
\\030\000\183\000\031\000\183\000\038\000\183\000\042\000\183\000\
\\046\000\183\000\000\000\
\\001\000\002\000\184\000\013\000\184\000\016\000\184\000\017\000\184\000\
\\018\000\184\000\019\000\184\000\020\000\184\000\025\000\184\000\
\\026\000\184\000\027\000\184\000\028\000\184\000\029\000\184\000\
\\030\000\184\000\031\000\184\000\038\000\184\000\042\000\184\000\
\\046\000\184\000\000\000\
\\001\000\002\000\185\000\013\000\185\000\016\000\185\000\017\000\185\000\
\\018\000\185\000\019\000\185\000\020\000\185\000\025\000\185\000\
\\026\000\185\000\027\000\185\000\028\000\185\000\029\000\185\000\
\\030\000\185\000\031\000\185\000\038\000\185\000\042\000\185\000\
\\046\000\185\000\000\000\
\\001\000\002\000\186\000\013\000\186\000\016\000\186\000\017\000\186\000\
\\018\000\186\000\019\000\186\000\020\000\186\000\025\000\186\000\
\\026\000\186\000\027\000\186\000\028\000\186\000\029\000\186\000\
\\030\000\186\000\031\000\186\000\038\000\186\000\042\000\186\000\
\\046\000\186\000\000\000\
\\001\000\002\000\192\000\013\000\192\000\020\000\192\000\031\000\064\000\
\\038\000\192\000\042\000\192\000\000\000\
\\001\000\002\000\193\000\013\000\193\000\016\000\074\000\017\000\073\000\
\\018\000\070\000\019\000\069\000\020\000\193\000\025\000\063\000\
\\026\000\062\000\027\000\061\000\028\000\060\000\029\000\059\000\
\\030\000\058\000\031\000\078\000\038\000\193\000\042\000\193\000\
\\046\000\068\000\000\000\
\\001\000\002\000\194\000\013\000\194\000\020\000\194\000\031\000\064\000\
\\038\000\194\000\042\000\194\000\000\000\
\\001\000\002\000\195\000\013\000\195\000\016\000\074\000\017\000\073\000\
\\018\000\070\000\019\000\069\000\020\000\195\000\025\000\063\000\
\\026\000\062\000\027\000\061\000\028\000\060\000\029\000\059\000\
\\030\000\058\000\031\000\078\000\038\000\195\000\042\000\195\000\
\\046\000\068\000\000\000\
\\001\000\002\000\196\000\013\000\196\000\020\000\196\000\031\000\064\000\
\\038\000\196\000\042\000\196\000\000\000\
\\001\000\002\000\197\000\013\000\197\000\020\000\197\000\031\000\197\000\
\\038\000\197\000\042\000\197\000\000\000\
\\001\000\002\000\198\000\013\000\198\000\020\000\198\000\031\000\198\000\
\\038\000\198\000\042\000\198\000\000\000\
\\001\000\002\000\199\000\013\000\199\000\020\000\199\000\031\000\199\000\
\\038\000\199\000\042\000\199\000\000\000\
\\001\000\002\000\200\000\013\000\200\000\020\000\200\000\031\000\200\000\
\\038\000\200\000\042\000\200\000\000\000\
\\001\000\002\000\201\000\013\000\201\000\020\000\201\000\025\000\063\000\
\\026\000\062\000\027\000\061\000\028\000\060\000\029\000\059\000\
\\030\000\058\000\031\000\201\000\038\000\201\000\042\000\201\000\000\000\
\\001\000\002\000\202\000\013\000\202\000\020\000\202\000\025\000\202\000\
\\026\000\202\000\027\000\202\000\028\000\202\000\029\000\202\000\
\\030\000\202\000\031\000\202\000\038\000\202\000\042\000\202\000\000\000\
\\001\000\002\000\203\000\013\000\203\000\020\000\203\000\025\000\203\000\
\\026\000\203\000\027\000\203\000\028\000\203\000\029\000\203\000\
\\030\000\203\000\031\000\203\000\038\000\203\000\042\000\203\000\000\000\
\\001\000\002\000\204\000\013\000\204\000\020\000\204\000\025\000\204\000\
\\026\000\204\000\027\000\204\000\028\000\204\000\029\000\204\000\
\\030\000\204\000\031\000\204\000\038\000\204\000\042\000\204\000\000\000\
\\001\000\002\000\205\000\013\000\205\000\020\000\205\000\031\000\205\000\
\\038\000\205\000\042\000\205\000\000\000\
\\001\000\002\000\206\000\013\000\206\000\016\000\074\000\017\000\073\000\
\\018\000\070\000\019\000\069\000\020\000\206\000\031\000\206\000\
\\038\000\206\000\042\000\206\000\046\000\068\000\000\000\
\\001\000\002\000\207\000\013\000\207\000\016\000\074\000\017\000\073\000\
\\020\000\207\000\025\000\207\000\026\000\207\000\027\000\207\000\
\\028\000\207\000\029\000\207\000\030\000\207\000\031\000\207\000\
\\038\000\207\000\042\000\207\000\000\000\
\\001\000\002\000\208\000\013\000\208\000\016\000\074\000\017\000\073\000\
\\020\000\208\000\025\000\208\000\026\000\208\000\027\000\208\000\
\\028\000\208\000\029\000\208\000\030\000\208\000\031\000\208\000\
\\038\000\208\000\042\000\208\000\000\000\
\\001\000\002\000\209\000\013\000\209\000\016\000\074\000\017\000\073\000\
\\018\000\070\000\019\000\069\000\020\000\209\000\025\000\209\000\
\\026\000\209\000\027\000\209\000\028\000\209\000\029\000\209\000\
\\030\000\209\000\031\000\209\000\038\000\209\000\042\000\209\000\
\\046\000\068\000\000\000\
\\001\000\002\000\210\000\013\000\210\000\016\000\074\000\017\000\073\000\
\\018\000\070\000\019\000\069\000\020\000\210\000\025\000\210\000\
\\026\000\210\000\027\000\210\000\028\000\210\000\029\000\210\000\
\\030\000\210\000\031\000\210\000\038\000\210\000\042\000\210\000\
\\046\000\068\000\000\000\
\\001\000\002\000\211\000\013\000\211\000\020\000\211\000\025\000\211\000\
\\026\000\211\000\027\000\211\000\028\000\211\000\029\000\211\000\
\\030\000\211\000\031\000\211\000\038\000\211\000\042\000\211\000\000\000\
\\001\000\002\000\212\000\013\000\212\000\020\000\212\000\025\000\212\000\
\\026\000\212\000\027\000\212\000\028\000\212\000\029\000\212\000\
\\030\000\212\000\031\000\212\000\038\000\212\000\042\000\212\000\000\000\
\\001\000\002\000\213\000\013\000\213\000\020\000\213\000\025\000\213\000\
\\026\000\213\000\027\000\213\000\028\000\213\000\029\000\213\000\
\\030\000\213\000\031\000\213\000\038\000\213\000\042\000\213\000\000\000\
\\001\000\002\000\214\000\013\000\214\000\020\000\214\000\025\000\214\000\
\\026\000\214\000\027\000\214\000\028\000\214\000\029\000\214\000\
\\030\000\214\000\031\000\214\000\038\000\214\000\042\000\214\000\000\000\
\\001\000\002\000\066\000\013\000\127\000\000\000\
\\001\000\002\000\066\000\020\000\163\000\000\000\
\\001\000\002\000\066\000\020\000\167\000\000\000\
\\001\000\002\000\066\000\038\000\090\000\000\000\
\\001\000\002\000\066\000\042\000\065\000\000\000\
\\001\000\002\000\079\000\013\000\129\000\016\000\074\000\017\000\073\000\
\\018\000\070\000\019\000\069\000\025\000\063\000\026\000\062\000\
\\027\000\061\000\028\000\060\000\029\000\059\000\030\000\058\000\
\\031\000\078\000\046\000\068\000\000\000\
\\001\000\002\000\079\000\016\000\074\000\017\000\073\000\018\000\070\000\
\\019\000\069\000\020\000\161\000\025\000\063\000\026\000\062\000\
\\027\000\061\000\028\000\060\000\029\000\059\000\030\000\058\000\
\\031\000\078\000\046\000\068\000\000\000\
\\001\000\002\000\079\000\016\000\074\000\017\000\073\000\018\000\070\000\
\\019\000\069\000\020\000\166\000\025\000\063\000\026\000\062\000\
\\027\000\061\000\028\000\060\000\029\000\059\000\030\000\058\000\
\\031\000\078\000\046\000\068\000\000\000\
\\001\000\002\000\079\000\016\000\074\000\017\000\073\000\018\000\070\000\
\\019\000\069\000\025\000\063\000\026\000\062\000\027\000\061\000\
\\028\000\060\000\029\000\059\000\030\000\058\000\031\000\078\000\
\\038\000\158\000\042\000\158\000\046\000\068\000\000\000\
\\001\000\003\000\000\000\000\000\
\\001\000\003\000\143\000\000\000\
\\001\000\003\000\144\000\000\000\
\\001\000\003\000\145\000\000\000\
\\001\000\003\000\154\000\039\000\154\000\040\000\154\000\043\000\154\000\000\000\
\\001\000\003\000\155\000\039\000\155\000\040\000\155\000\043\000\155\000\000\000\
\\001\000\004\000\053\000\000\000\
\\001\000\005\000\187\000\009\000\187\000\016\000\187\000\024\000\187\000\
\\045\000\187\000\000\000\
\\001\000\005\000\188\000\009\000\188\000\016\000\188\000\024\000\188\000\
\\045\000\188\000\000\000\
\\001\000\005\000\189\000\009\000\189\000\016\000\189\000\024\000\189\000\
\\045\000\189\000\000\000\
\\001\000\005\000\190\000\009\000\190\000\016\000\190\000\024\000\190\000\
\\045\000\190\000\000\000\
\\001\000\005\000\191\000\009\000\191\000\016\000\191\000\024\000\191\000\
\\045\000\191\000\000\000\
\\001\000\005\000\215\000\009\000\215\000\016\000\215\000\024\000\215\000\
\\032\000\215\000\033\000\215\000\034\000\215\000\045\000\215\000\000\000\
\\001\000\005\000\216\000\009\000\216\000\016\000\216\000\024\000\216\000\
\\032\000\216\000\033\000\216\000\034\000\216\000\045\000\216\000\000\000\
\\001\000\005\000\217\000\009\000\217\000\016\000\217\000\024\000\217\000\
\\032\000\217\000\033\000\217\000\034\000\217\000\045\000\217\000\000\000\
\\001\000\005\000\218\000\009\000\218\000\016\000\218\000\024\000\218\000\
\\032\000\218\000\033\000\218\000\034\000\218\000\045\000\218\000\000\000\
\\001\000\005\000\219\000\009\000\219\000\016\000\219\000\024\000\219\000\
\\032\000\219\000\033\000\219\000\034\000\219\000\045\000\219\000\000\000\
\\001\000\005\000\220\000\009\000\220\000\016\000\220\000\024\000\220\000\
\\032\000\220\000\033\000\220\000\034\000\220\000\045\000\220\000\000\000\
\\001\000\005\000\004\000\000\000\
\\001\000\005\000\022\000\015\000\156\000\035\000\020\000\036\000\019\000\
\\037\000\018\000\041\000\017\000\000\000\
\\001\000\005\000\022\000\015\000\021\000\035\000\020\000\036\000\019\000\
\\037\000\018\000\041\000\017\000\000\000\
\\001\000\005\000\025\000\000\000\
\\001\000\005\000\044\000\009\000\043\000\016\000\042\000\024\000\041\000\
\\032\000\040\000\033\000\039\000\034\000\038\000\045\000\037\000\000\000\
\\001\000\005\000\044\000\009\000\052\000\000\000\
\\001\000\005\000\044\000\009\000\083\000\016\000\042\000\024\000\041\000\
\\045\000\037\000\000\000\
\\001\000\005\000\044\000\009\000\101\000\032\000\040\000\033\000\039\000\
\\034\000\038\000\000\000\
\\001\000\006\000\097\000\044\000\096\000\000\000\
\\001\000\011\000\003\000\000\000\
\\001\000\012\000\149\000\014\000\149\000\000\000\
\\001\000\012\000\150\000\014\000\150\000\000\000\
\\001\000\012\000\012\000\014\000\147\000\000\000\
\\001\000\012\000\012\000\014\000\011\000\000\000\
\\001\000\013\000\128\000\016\000\074\000\017\000\073\000\000\000\
\\001\000\013\000\128\000\016\000\074\000\017\000\073\000\025\000\063\000\
\\026\000\062\000\027\000\061\000\028\000\060\000\029\000\059\000\
\\030\000\058\000\000\000\
\\001\000\013\000\129\000\000\000\
\\001\000\013\000\129\000\016\000\074\000\017\000\073\000\018\000\070\000\
\\019\000\069\000\046\000\068\000\000\000\
\\001\000\014\000\146\000\000\000\
\\001\000\014\000\148\000\000\000\
\\001\000\014\000\011\000\000\000\
\\001\000\015\000\157\000\000\000\
\\001\000\015\000\027\000\000\000\
\\001\000\016\000\074\000\017\000\073\000\020\000\162\000\025\000\063\000\
\\026\000\062\000\027\000\061\000\028\000\060\000\029\000\059\000\
\\030\000\058\000\000\000\
\\001\000\016\000\074\000\017\000\073\000\020\000\165\000\025\000\063\000\
\\026\000\062\000\027\000\061\000\028\000\060\000\029\000\059\000\
\\030\000\058\000\000\000\
\\001\000\016\000\074\000\017\000\073\000\025\000\063\000\026\000\062\000\
\\027\000\061\000\028\000\060\000\029\000\059\000\030\000\058\000\000\000\
\\001\000\020\000\158\000\000\000\
\\001\000\020\000\164\000\000\000\
\\001\000\020\000\168\000\000\000\
\\001\000\020\000\169\000\000\000\
\\001\000\020\000\170\000\000\000\
\\001\000\020\000\171\000\000\000\
\\001\000\020\000\026\000\000\000\
\\001\000\020\000\132\000\000\000\
\\001\000\020\000\133\000\000\000\
\\001\000\021\000\151\000\000\000\
\\001\000\021\000\152\000\023\000\055\000\000\000\
\\001\000\021\000\153\000\000\000\
\\001\000\021\000\054\000\000\000\
\\001\000\022\000\005\000\000\000\
\\001\000\024\000\086\000\000\000\
\\001\000\038\000\091\000\000\000\
\\001\000\039\000\136\000\000\000\
\\001\000\039\000\137\000\000\000\
\\001\000\040\000\140\000\000\000\
\\001\000\040\000\141\000\000\000\
\\001\000\042\000\080\000\000\000\
\\001\000\043\000\134\000\000\000\
\\001\000\043\000\135\000\000\000\
\"
val actionRowNumbers =
"\076\000\067\000\106\000\080\000\
\\052\000\079\000\085\000\087\000\
\\050\000\069\000\070\000\086\000\
\\051\000\099\000\089\000\071\000\
\\071\000\071\000\072\000\054\000\
\\055\000\102\000\105\000\103\000\
\\068\000\053\000\026\000\039\000\
\\021\000\044\000\011\000\006\000\
\\092\000\048\000\113\000\073\000\
\\071\000\028\000\027\000\012\000\
\\107\000\071\000\001\000\043\000\
\\108\000\042\000\091\000\047\000\
\\093\000\094\000\072\000\071\000\
\\075\000\070\000\088\000\074\000\
\\066\000\065\000\064\000\063\000\
\\062\000\061\000\074\000\087\000\
\\071\000\073\000\060\000\059\000\
\\058\000\073\000\073\000\057\000\
\\056\000\073\000\073\000\071\000\
\\074\000\071\000\087\000\015\000\
\\016\000\073\000\030\000\031\000\
\\013\000\040\000\082\000\045\000\
\\087\000\087\000\083\000\041\000\
\\090\000\046\000\100\000\101\000\
\\104\000\036\000\037\000\071\000\
\\022\000\023\000\114\000\017\000\
\\018\000\007\000\008\000\002\000\
\\003\000\032\000\034\000\009\000\
\\010\000\004\000\005\000\038\000\
\\033\000\035\000\024\000\025\000\
\\019\000\020\000\115\000\081\000\
\\084\000\029\000\014\000\000\000\
\\109\000\110\000\078\000\077\000\
\\097\000\098\000\087\000\087\000\
\\111\000\112\000\095\000\096\000\
\\049\000"
val gotoT =
"\
\\001\000\140\000\000\000\
\\000\000\
\\000\000\
\\002\000\008\000\003\000\007\000\004\000\006\000\005\000\005\000\
\\010\000\004\000\000\000\
\\000\000\
\\004\000\011\000\005\000\005\000\000\000\
\\000\000\
\\010\000\012\000\000\000\
\\000\000\
\\011\000\014\000\013\000\013\000\000\000\
\\006\000\022\000\007\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\034\000\009\000\033\000\014\000\032\000\015\000\031\000\
\\016\000\030\000\017\000\029\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\000\000\
\\008\000\044\000\009\000\033\000\014\000\032\000\015\000\031\000\
\\016\000\030\000\017\000\043\000\018\000\028\000\019\000\027\000\
\\020\000\026\000\000\000\
\\009\000\047\000\014\000\046\000\015\000\031\000\016\000\030\000\
\\017\000\045\000\018\000\028\000\019\000\027\000\020\000\026\000\000\000\
\\008\000\049\000\009\000\048\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\054\000\013\000\013\000\000\000\
\\000\000\
\\021\000\055\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\024\000\065\000\000\000\
\\021\000\070\000\023\000\069\000\000\000\
\\021\000\075\000\023\000\074\000\024\000\073\000\000\000\
\\000\000\
\\009\000\080\000\016\000\079\000\000\000\
\\009\000\083\000\014\000\032\000\015\000\031\000\016\000\030\000\
\\019\000\027\000\020\000\082\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\087\000\014\000\086\000\015\000\031\000\016\000\030\000\
\\017\000\085\000\018\000\028\000\019\000\027\000\020\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\070\000\023\000\069\000\000\000\
\\021\000\075\000\023\000\074\000\024\000\073\000\000\000\
\\000\000\
\\000\000\
\\009\000\090\000\000\000\
\\009\000\093\000\014\000\092\000\015\000\031\000\016\000\030\000\
\\017\000\091\000\018\000\028\000\019\000\027\000\020\000\026\000\000\000\
\\000\000\
\\007\000\096\000\000\000\
\\000\000\
\\009\000\098\000\019\000\097\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\101\000\019\000\100\000\000\000\
\\010\000\102\000\000\000\
\\009\000\104\000\014\000\032\000\015\000\031\000\016\000\030\000\
\\018\000\103\000\019\000\027\000\020\000\026\000\000\000\
\\009\000\106\000\016\000\105\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\108\000\015\000\107\000\016\000\030\000\000\000\
\\009\000\110\000\014\000\109\000\015\000\031\000\016\000\030\000\000\000\
\\000\000\
\\000\000\
\\009\000\112\000\016\000\111\000\000\000\
\\009\000\114\000\015\000\113\000\016\000\030\000\000\000\
\\009\000\117\000\014\000\116\000\015\000\031\000\016\000\030\000\
\\019\000\115\000\000\000\
\\009\000\119\000\019\000\118\000\000\000\
\\009\000\121\000\014\000\032\000\015\000\031\000\016\000\030\000\
\\018\000\120\000\019\000\027\000\020\000\026\000\000\000\
\\010\000\122\000\000\000\
\\000\000\
\\000\000\
\\009\000\124\000\014\000\123\000\015\000\031\000\016\000\030\000\000\000\
\\021\000\055\000\000\000\
\\021\000\075\000\023\000\074\000\024\000\073\000\000\000\
\\000\000\
\\000\000\
\\021\000\070\000\023\000\069\000\000\000\
\\021\000\075\000\023\000\074\000\024\000\073\000\000\000\
\\010\000\128\000\000\000\
\\010\000\129\000\000\000\
\\000\000\
\\000\000\
\\021\000\070\000\023\000\069\000\000\000\
\\021\000\075\000\023\000\074\000\024\000\073\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\087\000\014\000\032\000\015\000\031\000\016\000\030\000\
\\017\000\085\000\018\000\028\000\019\000\027\000\020\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\075\000\023\000\074\000\024\000\073\000\000\000\
\\000\000\
\\000\000\
\\024\000\065\000\000\000\
\\024\000\073\000\000\000\
\\023\000\069\000\000\000\
\\023\000\074\000\024\000\073\000\000\000\
\\000\000\
\\000\000\
\\024\000\065\000\000\000\
\\024\000\073\000\000\000\
\\000\000\
\\023\000\069\000\000\000\
\\023\000\074\000\024\000\073\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\075\000\023\000\074\000\024\000\073\000\000\000\
\\000\000\
\\023\000\069\000\000\000\
\\023\000\074\000\024\000\073\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\136\000\000\000\
\\010\000\137\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 141
val numrules = 78
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUMBER of unit ->  (int) | IDE of unit ->  (string)
 | MultiOP of unit ->  (OP) | AddOP of unit ->  (OP)
 | Type of unit ->  (Type) | RelOP of unit ->  (Relop)
 | Comparison of unit ->  (Comparison)
 | BoolFactor of unit ->  (BoolFactor)
 | Boolterm of unit ->  (Boolterm)
 | BoolExpression of unit ->  (BoolExpression)
 | NoMultiExpression of unit ->  (NoMultiExpression)
 | NoAddExpression of unit ->  (NoAddExpression)
 | IntExpression of unit ->  (IntExpression)
 | Command of unit ->  (Command)
 | parallelCmdList1 of unit ->  (Command list)
 | parallelCmdList of unit ->  (Command list)
 | CommandList of unit ->  (CommandList) | Var of unit ->  (string)
 | Variable of unit ->  (Variable)
 | parallelVarList of unit ->  (string list)
 | VariableList of unit ->  (string list)
 | Declaration of unit ->  (string list*Type)
 | parallelDecSeq of unit ->  ( ( string list * Type )  list)
 | DeclarationSeq of unit ->  (DeclarationSeq)
 | Block of unit ->  (Block) | begin of unit ->  (Program)
end
type svalue = MlyValue.svalue
type result = Program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 11) => true | (T 10) => true | (T 5) => true | (T 43) => true | 
(T 34) => true | (T 35) => true | (T 36) => true | (T 38) => true | 
(T 37) => true | (T 40) => true | (T 41) => true | (T 42) => true | 
(T 39) => true | (T 31) => true | (T 32) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 2) => true | _ => false
val showTerminal =
fn (T 0) => "CARET"
  | (T 1) => "DVBAR"
  | (T 2) => "EOF"
  | (T 3) => "EQUALS"
  | (T 4) => "IDE"
  | (T 5) => "INT"
  | (T 6) => "ILLCH"
  | (T 7) => "INPUT"
  | (T 8) => "LPAR"
  | (T 9) => "NEW"
  | (T 10) => "PRO"
  | (T 11) => "VAR"
  | (T 12) => "RPAR"
  | (T 13) => "LFLO"
  | (T 14) => "RFLO"
  | (T 15) => "PLUS"
  | (T 16) => "MINUS"
  | (T 17) => "TIMES"
  | (T 18) => "DIV"
  | (T 19) => "SEMICOLON"
  | (T 20) => "COLON"
  | (T 21) => "DOUBLECOLON"
  | (T 22) => "COMMA"
  | (T 23) => "NUMBER"
  | (T 24) => "LT"
  | (T 25) => "GT"
  | (T 26) => "LEQ"
  | (T 27) => "GEQ"
  | (T 28) => "NEQ"
  | (T 29) => "EQ"
  | (T 30) => "AND"
  | (T 31) => "TT"
  | (T 32) => "FF"
  | (T 33) => "NOT"
  | (T 34) => "READ"
  | (T 35) => "WRITE"
  | (T 36) => "IF"
  | (T 37) => "THEN"
  | (T 38) => "ELSE"
  | (T 39) => "ENDIF"
  | (T 40) => "WHI"
  | (T 41) => "DO"
  | (T 42) => "ENDWH"
  | (T 43) => "BOOL"
  | (T 44) => "NEG"
  | (T 45) => "MOD"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39)
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 3) $$ (T 2) $$ (T 
1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Block Block1, _, Block1right)) :: _ :: ( _,
 ( MlyValue.IDE IDE1, _, _)) :: ( _, ( _, PRO1left, _)) :: rest671))
 => let val  result = MlyValue.begin (fn _ => let val  (IDE as IDE1) =
 IDE1 ()
 val  (Block as Block1) = Block1 ()
 in (
(let val _ = nullifyI() and _ = nullifyB() in PROG (IDE, Block) end))

end)
 in ( LrTable.NT 0, ( result, PRO1left, Block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.CommandList CommandList1, _, 
CommandList1right)) :: ( _, ( MlyValue.DeclarationSeq DeclarationSeq1,
 DeclarationSeq1left, _)) :: rest671)) => let val  result = 
MlyValue.Block (fn _ => let val  (DeclarationSeq as DeclarationSeq1) =
 DeclarationSeq1 ()
 val  (CommandList as CommandList1) = CommandList1 ()
 in ((BLK_DEC (DeclarationSeq, CommandList)))
end)
 in ( LrTable.NT 1, ( result, DeclarationSeq1left, CommandList1right),
 rest671)
end
|  ( 2, ( ( _, ( MlyValue.CommandList CommandList1, CommandList1left, 
CommandList1right)) :: rest671)) => let val  result = MlyValue.Block
 (fn _ => let val  (CommandList as CommandList1) = CommandList1 ()
 in ((BLK_NODEC CommandList))
end)
 in ( LrTable.NT 1, ( result, CommandList1left, CommandList1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.parallelDecSeq parallelDecSeq1, 
parallelDecSeq1left, parallelDecSeq1right)) :: rest671)) => let val  
result = MlyValue.DeclarationSeq (fn _ => let val  (parallelDecSeq as 
parallelDecSeq1) = parallelDecSeq1 ()
 in ((DEC parallelDecSeq))
end)
 in ( LrTable.NT 2, ( result, parallelDecSeq1left, 
parallelDecSeq1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Declaration Declaration1, Declaration1left, 
Declaration1right)) :: rest671)) => let val  result = 
MlyValue.parallelDecSeq (fn _ => let val  (Declaration as Declaration1
) = Declaration1 ()
 in (([Declaration]))
end)
 in ( LrTable.NT 3, ( result, Declaration1left, Declaration1right), 
rest671)
end
|  ( 5, ( ( _, ( MlyValue.parallelDecSeq parallelDecSeq1, _, 
parallelDecSeq1right)) :: ( _, ( MlyValue.Declaration Declaration1, 
Declaration1left, _)) :: rest671)) => let val  result = 
MlyValue.parallelDecSeq (fn _ => let val  (Declaration as Declaration1
) = Declaration1 ()
 val  (parallelDecSeq as parallelDecSeq1) = parallelDecSeq1 ()
 in ((Declaration::parallelDecSeq))
end)
 in ( LrTable.NT 3, ( result, Declaration1left, parallelDecSeq1right),
 rest671)
end
|  ( 6, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: _ :: ( _, ( 
MlyValue.VariableList VariableList1, _, _)) :: ( _, ( _, VAR1left, _))
 :: rest671)) => let val  result = MlyValue.Declaration (fn _ => let
 val  (VariableList as VariableList1) = VariableList1 ()
 in (
(let val _ = List.app addI VariableList in (VariableList, INT) end))

end)
 in ( LrTable.NT 4, ( result, VAR1left, SEMICOLON1right), rest671)
end
|  ( 7, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: _ :: ( _, ( 
MlyValue.VariableList VariableList1, _, _)) :: ( _, ( _, VAR1left, _))
 :: rest671)) => let val  result = MlyValue.Declaration (fn _ => let
 val  (VariableList as VariableList1) = VariableList1 ()
 in (
(let val _ = List.app addB VariableList in (VariableList, BOOL) end))

end)
 in ( LrTable.NT 4, ( result, VAR1left, SEMICOLON1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.parallelVarList parallelVarList1, 
parallelVarList1left, parallelVarList1right)) :: rest671)) => let val 
 result = MlyValue.VariableList (fn _ => let val  (parallelVarList as 
parallelVarList1) = parallelVarList1 ()
 in ((parallelVarList))
end)
 in ( LrTable.NT 5, ( result, parallelVarList1left, 
parallelVarList1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.IDE IDE1, IDE1left, IDE1right)) :: rest671))
 => let val  result = MlyValue.parallelVarList (fn _ => let val  (IDE
 as IDE1) = IDE1 ()
 in (([IDE]))
end)
 in ( LrTable.NT 6, ( result, IDE1left, IDE1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.parallelVarList parallelVarList1, _, 
parallelVarList1right)) :: _ :: ( _, ( MlyValue.IDE IDE1, IDE1left, _)
) :: rest671)) => let val  result = MlyValue.parallelVarList (fn _ =>
 let val  (IDE as IDE1) = IDE1 ()
 val  (parallelVarList as parallelVarList1) = parallelVarList1 ()
 in ((IDE::parallelVarList))
end)
 in ( LrTable.NT 6, ( result, IDE1left, parallelVarList1right), 
rest671)
end
|  ( 11, ( ( _, ( _, _, RFLO1right)) :: ( _, ( 
MlyValue.parallelCmdList parallelCmdList1, _, _)) :: ( _, ( _, 
LFLO1left, _)) :: rest671)) => let val  result = MlyValue.CommandList
 (fn _ => let val  (parallelCmdList as parallelCmdList1) = 
parallelCmdList1 ()
 in ((SEQ parallelCmdList))
end)
 in ( LrTable.NT 9, ( result, LFLO1left, RFLO1right), rest671)
end
|  ( 12, ( ( _, ( _, _, RFLO1right)) :: ( _, ( _, LFLO1left, _)) :: 
rest671)) => let val  result = MlyValue.CommandList (fn _ => ((SEQ [])
))
 in ( LrTable.NT 9, ( result, LFLO1left, RFLO1right), rest671)
end
|  ( 13, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.Command 
Command1, Command1left, _)) :: rest671)) => let val  result = 
MlyValue.parallelCmdList (fn _ => let val  (Command as Command1) = 
Command1 ()
 in (([Command]))
end)
 in ( LrTable.NT 10, ( result, Command1left, SEMICOLON1right), rest671
)
end
|  ( 14, ( ( _, ( MlyValue.parallelCmdList parallelCmdList1, _, 
parallelCmdList1right)) :: _ :: ( _, ( MlyValue.Command Command1, 
Command1left, _)) :: rest671)) => let val  result = 
MlyValue.parallelCmdList (fn _ => let val  (Command as Command1) = 
Command1 ()
 val  (parallelCmdList as parallelCmdList1) = parallelCmdList1 ()
 in ((Command :: parallelCmdList))
end)
 in ( LrTable.NT 10, ( result, Command1left, parallelCmdList1right), 
rest671)
end
|  ( 15, ( ( _, ( MlyValue.Var Var1, (Varleft as Var1left), (Varright
 as Var1right))) :: rest671)) => let val  result = MlyValue.Variable
 (fn _ => let val  (Var as Var1) = Var1 ()
 in (
(if findB Var = NONE andalso findI Var = NONE then let val _ = badCh (Var, Varleft, Varright, "any type") in (Int Var) end else if findB Var = NONE then (Int Var) else (Bool Var) )
)
end)
 in ( LrTable.NT 7, ( result, Var1left, Var1right), rest671)
end
|  ( 16, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Var Var1, _, _
)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Var (fn _ => let val  (Var as Var1) = Var1 ()
 in ((Var))
end)
 in ( LrTable.NT 8, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.IDE IDE1, (IDEleft as IDE1left), (IDEright
 as IDE1right))) :: rest671)) => let val  result = MlyValue.Var (fn _
 => let val  (IDE as IDE1) = IDE1 ()
 in (
(let val _ = PositionTable.add(IDE, (IDEleft, IDEright)) in IDE end))

end)
 in ( LrTable.NT 8, ( result, IDE1left, IDE1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.Var Var1, Varleft, (Varright as Var1right))
) :: _ :: ( _, ( MlyValue.IDE IDE1, (IDEleft as IDE1left), IDEright))
 :: rest671)) => let val  result = MlyValue.Command (fn _ => let val 
 (IDE as IDE1) = IDE1 ()
 val  (Var as Var1) = Var1 ()
 in (
(if findB IDE = NONE andalso findI IDE = NONE then let val _ = badCh (IDE, IDEleft, IDEright, "any type") in SET_INT (Int IDE, (IEXP_sub (IEXP1_sub (IVAL (Int Var))))) end else if findB IDE = NONE then let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in SET_INT (Int IDE, (IEXP_sub (IEXP1_sub (IVAL (Int Var))))) end else let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in SET_BOOL (Bool IDE, (NO_OR (NO_AND (BFACT (BVAL (Bool Var)))))) end)
)
end)
 in ( LrTable.NT 12, ( result, IDE1left, Var1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.IntExpression IntExpression1, _, 
IntExpression1right)) :: _ :: ( _, ( MlyValue.IDE IDE1, (IDEleft as 
IDE1left), IDEright)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (IDE as IDE1) = IDE1 ()
 val  (IntExpression as IntExpression1) = IntExpression1 ()
 in (
(let val _ = if findI IDE = NONE then badCh (IDE, IDEleft, IDEright, "int") else () in SET_INT (Int IDE, IntExpression) end)
)
end)
 in ( LrTable.NT 12, ( result, IDE1left, IntExpression1right), rest671
)
end
|  ( 20, ( ( _, ( MlyValue.BoolExpression BoolExpression1, _, 
BoolExpression1right)) :: _ :: ( _, ( MlyValue.IDE IDE1, (IDEleft as 
IDE1left), IDEright)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (IDE as IDE1) = IDE1 ()
 val  (BoolExpression as BoolExpression1) = BoolExpression1 ()
 in (
(let val _ = if findB IDE = NONE then badCh (IDE, IDEleft, IDEright, "bool") else () in SET_BOOL (Bool IDE, BoolExpression) end)
)
end)
 in ( LrTable.NT 12, ( result, IDE1left, BoolExpression1right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.Variable Variable1, _, Variable1right)) :: 
( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Variable as Variable1) = Variable1
 ()
 in ((READ Variable))
end)
 in ( LrTable.NT 12, ( result, READ1left, Variable1right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.IntExpression IntExpression1, _, 
IntExpression1right)) :: ( _, ( _, WRITE1left, _)) :: rest671)) => let
 val  result = MlyValue.Command (fn _ => let val  (IntExpression as 
IntExpression1) = IntExpression1 ()
 in ((WRITEI IntExpression))
end)
 in ( LrTable.NT 12, ( result, WRITE1left, IntExpression1right), 
rest671)
end
|  ( 23, ( ( _, ( MlyValue.Var Var1, Varleft, (Varright as Var1right))
) :: ( _, ( _, WRITE1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Var as Var1) = Var1 ()
 in (
(if findB Var = NONE andalso findI Var = NONE then let val _ = badCh (Var, Varleft, Varright, "any type") in (WRITEI (IEXP_sub (IEXP1_sub (IVAL (Int Var))))) end else if findB Var = NONE then let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in (WRITEI (IEXP_sub (IEXP1_sub (IVAL (Int Var))))) end else let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in (WRITEB (NO_OR (NO_AND (BFACT (BVAL (Bool Var)))))) end)
)
end)
 in ( LrTable.NT 12, ( result, WRITE1left, Var1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.BoolExpression BoolExpression1, _, 
BoolExpression1right)) :: ( _, ( _, WRITE1left, _)) :: rest671)) =>
 let val  result = MlyValue.Command (fn _ => let val  (BoolExpression
 as BoolExpression1) = BoolExpression1 ()
 in ((WRITEB BoolExpression))
end)
 in ( LrTable.NT 12, ( result, WRITE1left, BoolExpression1right), 
rest671)
end
|  ( 25, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.CommandList 
CommandList2, _, _)) :: _ :: ( _, ( MlyValue.CommandList CommandList1,
 _, _)) :: _ :: ( _, ( MlyValue.BoolExpression BoolExpression1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (BoolExpression as BoolExpression1)
 = BoolExpression1 ()
 val  CommandList1 = CommandList1 ()
 val  CommandList2 = CommandList2 ()
 in ((ITE (BoolExpression, CommandList1, CommandList2)))
end)
 in ( LrTable.NT 12, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 26, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.CommandList 
CommandList2, _, _)) :: _ :: ( _, ( MlyValue.CommandList CommandList1,
 _, _)) :: _ :: ( _, ( MlyValue.Variable Variable1, _, _)) :: ( _, ( _
, IF1left, _)) :: rest671)) => let val  result = MlyValue.Command (fn
 _ => let val  (Variable as Variable1) = Variable1 ()
 val  CommandList1 = CommandList1 ()
 val  CommandList2 = CommandList2 ()
 in (
(ITE ((NO_OR (NO_AND (BFACT (BVAL Variable)))), CommandList1, CommandList2))
)
end)
 in ( LrTable.NT 12, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 27, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.CommandList 
CommandList1, _, _)) :: _ :: ( _, ( MlyValue.BoolExpression 
BoolExpression1, _, _)) :: ( _, ( _, WHI1left, _)) :: rest671)) => let
 val  result = MlyValue.Command (fn _ => let val  (BoolExpression as 
BoolExpression1) = BoolExpression1 ()
 val  (CommandList as CommandList1) = CommandList1 ()
 in ((WH (BoolExpression, CommandList)))
end)
 in ( LrTable.NT 12, ( result, WHI1left, ENDWH1right), rest671)
end
|  ( 28, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.CommandList 
CommandList1, _, _)) :: _ :: ( _, ( MlyValue.Variable Variable1, _, _)
) :: ( _, ( _, WHI1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Variable as Variable1) = Variable1
 ()
 val  (CommandList as CommandList1) = CommandList1 ()
 in ((WH ((NO_OR (NO_AND (BFACT (BVAL Variable)))), CommandList)))
end
)
 in ( LrTable.NT 12, ( result, WHI1left, ENDWH1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.NoAddExpression NoAddExpression1, _, 
NoAddExpression1right)) :: ( _, ( MlyValue.AddOP AddOP1, _, _)) :: ( _
, ( MlyValue.IntExpression IntExpression1, IntExpression1left, _)) :: 
rest671)) => let val  result = MlyValue.IntExpression (fn _ => let
 val  (IntExpression as IntExpression1) = IntExpression1 ()
 val  (AddOP as AddOP1) = AddOP1 ()
 val  (NoAddExpression as NoAddExpression1) = NoAddExpression1 ()
 in (
(IEXP (IntExpression,                          AddOP, NoAddExpression))
)
end)
 in ( LrTable.NT 13, ( result, IntExpression1left, 
NoAddExpression1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.Var Var1, Varleft, (Varright as Var1right))
) :: ( _, ( MlyValue.AddOP AddOP1, _, _)) :: ( _, ( 
MlyValue.IntExpression IntExpression1, IntExpression1left, _)) :: 
rest671)) => let val  result = MlyValue.IntExpression (fn _ => let
 val  (IntExpression as IntExpression1) = IntExpression1 ()
 val  (AddOP as AddOP1) = AddOP1 ()
 val  (Var as Var1) = Var1 ()
 in (
(let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in IEXP (IntExpression,         AddOP, (IEXP1_sub (IVAL (Int Var)))) end)
)
end)
 in ( LrTable.NT 13, ( result, IntExpression1left, Var1right), rest671
)
end
|  ( 31, ( ( _, ( MlyValue.NoAddExpression NoAddExpression1, _, 
NoAddExpression1right)) :: ( _, ( MlyValue.AddOP AddOP1, _, _)) :: ( _
, ( MlyValue.Var Var1, (Varleft as Var1left), Varright)) :: rest671))
 => let val  result = MlyValue.IntExpression (fn _ => let val  (Var
 as Var1) = Var1 ()
 val  (AddOP as AddOP1) = AddOP1 ()
 val  (NoAddExpression as NoAddExpression1) = NoAddExpression1 ()
 in (
(let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in IEXP ((IEXP_sub (IEXP1_sub (IVAL (Int Var)))),AddOP, NoAddExpression) end)
)
end)
 in ( LrTable.NT 13, ( result, Var1left, NoAddExpression1right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.Var Var2, Var2left, Var2right)) :: ( _, ( 
MlyValue.AddOP AddOP1, _, _)) :: ( _, ( MlyValue.Var Var1, Var1left, 
Var1right)) :: rest671)) => let val  result = MlyValue.IntExpression
 (fn _ => let val  Var1 = Var1 ()
 val  (AddOP as AddOP1) = AddOP1 ()
 val  Var2 = Var2 ()
 in (
(let val _ = if findI Var1 = NONE then badCh (Var1, Var1left, Var1right, "int") else if findI Var2 = NONE then badCh (Var2, Var2left, Var2right, "int") else () in IEXP ((IEXP_sub (IEXP1_sub (IVAL (Int Var1)))),AddOP,(IEXP1_sub (IVAL (Int Var2)))) end)
)
end)
 in ( LrTable.NT 13, ( result, Var1left, Var2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.NoAddExpression NoAddExpression1, 
NoAddExpression1left, NoAddExpression1right)) :: rest671)) => let val 
 result = MlyValue.IntExpression (fn _ => let val  (NoAddExpression
 as NoAddExpression1) = NoAddExpression1 ()
 in ((IEXP_sub NoAddExpression))
end)
 in ( LrTable.NT 13, ( result, NoAddExpression1left, 
NoAddExpression1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.NoMultiExpression NoMultiExpression1, _, 
NoMultiExpression1right)) :: ( _, ( MlyValue.MultiOP MultiOP1, _, _))
 :: ( _, ( MlyValue.NoAddExpression NoAddExpression1, 
NoAddExpression1left, _)) :: rest671)) => let val  result = 
MlyValue.NoAddExpression (fn _ => let val  (NoAddExpression as 
NoAddExpression1) = NoAddExpression1 ()
 val  (MultiOP as MultiOP1) = MultiOP1 ()
 val  (NoMultiExpression as NoMultiExpression1) = NoMultiExpression1
 ()
 in (
(IEXP1 (NoAddExpression,               MultiOP, NoMultiExpression)))

end)
 in ( LrTable.NT 14, ( result, NoAddExpression1left, 
NoMultiExpression1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.Var Var1, Varleft, (Varright as Var1right))
) :: ( _, ( MlyValue.MultiOP MultiOP1, _, _)) :: ( _, ( 
MlyValue.NoAddExpression NoAddExpression1, NoAddExpression1left, _))
 :: rest671)) => let val  result = MlyValue.NoAddExpression (fn _ =>
 let val  (NoAddExpression as NoAddExpression1) = NoAddExpression1 ()
 val  (MultiOP as MultiOP1) = MultiOP1 ()
 val  (Var as Var1) = Var1 ()
 in (
(let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in IEXP1 (NoAddExpression,               MultiOP, (IVAL (Int Var))) end)
)
end)
 in ( LrTable.NT 14, ( result, NoAddExpression1left, Var1right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.NoMultiExpression NoMultiExpression1, _, 
NoMultiExpression1right)) :: ( _, ( MlyValue.MultiOP MultiOP1, _, _))
 :: ( _, ( MlyValue.Var Var1, (Varleft as Var1left), Varright)) :: 
rest671)) => let val  result = MlyValue.NoAddExpression (fn _ => let
 val  (Var as Var1) = Var1 ()
 val  (MultiOP as MultiOP1) = MultiOP1 ()
 val  (NoMultiExpression as NoMultiExpression1) = NoMultiExpression1
 ()
 in (
(let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in IEXP1 (IEXP1_sub (IVAL (Int Var)),    MultiOP, NoMultiExpression) end)
)
end)
 in ( LrTable.NT 14, ( result, Var1left, NoMultiExpression1right), 
rest671)
end
|  ( 37, ( ( _, ( MlyValue.Var Var2, Var2left, Var2right)) :: ( _, ( 
MlyValue.MultiOP MultiOP1, _, _)) :: ( _, ( MlyValue.Var Var1, 
Var1left, Var1right)) :: rest671)) => let val  result = 
MlyValue.NoAddExpression (fn _ => let val  Var1 = Var1 ()
 val  (MultiOP as MultiOP1) = MultiOP1 ()
 val  Var2 = Var2 ()
 in (
(let val _ = if findI Var1 = NONE then badCh (Var1, Var1left, Var1right, "int") else if findI Var2 = NONE then badCh (Var2, Var2left, Var2right, "int") else () in IEXP1 (IEXP1_sub (IVAL (Int Var1)),    MultiOP, (IVAL (Int Var2))) end)
)
end)
 in ( LrTable.NT 14, ( result, Var1left, Var2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.NoMultiExpression NoMultiExpression1, 
NoMultiExpression1left, NoMultiExpression1right)) :: rest671)) => let
 val  result = MlyValue.NoAddExpression (fn _ => let val  (
NoMultiExpression as NoMultiExpression1) = NoMultiExpression1 ()
 in ((IEXP1_sub NoMultiExpression))
end)
 in ( LrTable.NT 14, ( result, NoMultiExpression1left, 
NoMultiExpression1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.NUMBER NUMBER1, NUMBER1left, NUMBER1right))
 :: rest671)) => let val  result = MlyValue.NoMultiExpression (fn _ =>
 let val  (NUMBER as NUMBER1) = NUMBER1 ()
 in ((NUM NUMBER))
end)
 in ( LrTable.NT 15, ( result, NUMBER1left, NUMBER1right), rest671)

end
|  ( 40, ( ( _, ( MlyValue.NUMBER NUMBER1, _, NUMBER1right)) :: ( _, (
 _, PLUS1left, _)) :: rest671)) => let val  result = 
MlyValue.NoMultiExpression (fn _ => let val  (NUMBER as NUMBER1) = 
NUMBER1 ()
 in ((NUM NUMBER))
end)
 in ( LrTable.NT 15, ( result, PLUS1left, NUMBER1right), rest671)
end
|  ( 41, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.IntExpression 
IntExpression1, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let
 val  result = MlyValue.NoMultiExpression (fn _ => let val  (
IntExpression as IntExpression1) = IntExpression1 ()
 in ((IBRAC IntExpression))
end)
 in ( LrTable.NT 15, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.NoMultiExpression NoMultiExpression1, _, 
NoMultiExpression1right)) :: ( _, ( _, NEG1left, _)) :: rest671)) =>
 let val  result = MlyValue.NoMultiExpression (fn _ => let val  (
NoMultiExpression as NoMultiExpression1) = NoMultiExpression1 ()
 in ((NEG NoMultiExpression))
end)
 in ( LrTable.NT 15, ( result, NEG1left, NoMultiExpression1right), 
rest671)
end
|  ( 43, ( ( _, ( MlyValue.Var Var1, Varleft, (Varright as Var1right))
) :: ( _, ( _, NEG1left, _)) :: rest671)) => let val  result = 
MlyValue.NoMultiExpression (fn _ => let val  (Var as Var1) = Var1 ()
 in (
(let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in NEG ((IVAL (Int Var))) end)
)
end)
 in ( LrTable.NT 15, ( result, NEG1left, Var1right), rest671)
end
|  ( 44, ( ( _, ( _, PLUS1left, PLUS1right)) :: rest671)) => let val  
result = MlyValue.AddOP (fn _ => ((PLUS)))
 in ( LrTable.NT 22, ( result, PLUS1left, PLUS1right), rest671)
end
|  ( 45, ( ( _, ( _, MINUS1left, MINUS1right)) :: rest671)) => let
 val  result = MlyValue.AddOP (fn _ => ((MINUS)))
 in ( LrTable.NT 22, ( result, MINUS1left, MINUS1right), rest671)
end
|  ( 46, ( ( _, ( _, TIMES1left, TIMES1right)) :: rest671)) => let
 val  result = MlyValue.MultiOP (fn _ => ((TIMES)))
 in ( LrTable.NT 23, ( result, TIMES1left, TIMES1right), rest671)
end
|  ( 47, ( ( _, ( _, DIV1left, DIV1right)) :: rest671)) => let val  
result = MlyValue.MultiOP (fn _ => ((DIV)))
 in ( LrTable.NT 23, ( result, DIV1left, DIV1right), rest671)
end
|  ( 48, ( ( _, ( _, MOD1left, MOD1right)) :: rest671)) => let val  
result = MlyValue.MultiOP (fn _ => ((MOD)))
 in ( LrTable.NT 23, ( result, MOD1left, MOD1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.Boolterm Boolterm1, _, Boolterm1right)) ::
 _ :: ( _, ( MlyValue.BoolExpression BoolExpression1, 
BoolExpression1left, _)) :: rest671)) => let val  result = 
MlyValue.BoolExpression (fn _ => let val  (BoolExpression as 
BoolExpression1) = BoolExpression1 ()
 val  (Boolterm as Boolterm1) = Boolterm1 ()
 in ((OR (BoolExpression, Boolterm)))
end)
 in ( LrTable.NT 16, ( result, BoolExpression1left, Boolterm1right), 
rest671)
end
|  ( 50, ( ( _, ( MlyValue.Var Var1, Varleft, (Varright as Var1right))
) :: _ :: ( _, ( MlyValue.BoolExpression BoolExpression1, 
BoolExpression1left, _)) :: rest671)) => let val  result = 
MlyValue.BoolExpression (fn _ => let val  (BoolExpression as 
BoolExpression1) = BoolExpression1 ()
 val  (Var as Var1) = Var1 ()
 in (
(let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in OR (BoolExpression, (NO_AND (BFACT (BVAL (Bool Var))))) end)
)
end)
 in ( LrTable.NT 16, ( result, BoolExpression1left, Var1right), 
rest671)
end
|  ( 51, ( ( _, ( MlyValue.Boolterm Boolterm1, _, Boolterm1right)) ::
 _ :: ( _, ( MlyValue.Var Var1, (Varleft as Var1left), Varright)) :: 
rest671)) => let val  result = MlyValue.BoolExpression (fn _ => let
 val  (Var as Var1) = Var1 ()
 val  (Boolterm as Boolterm1) = Boolterm1 ()
 in (
(let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in OR (NO_OR (NO_AND (BFACT (BVAL (Bool Var)))), Boolterm) end)
)
end)
 in ( LrTable.NT 16, ( result, Var1left, Boolterm1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.Var Var2, Var2left, Var2right)) :: _ :: ( _
, ( MlyValue.Var Var1, Var1left, Var1right)) :: rest671)) => let val  
result = MlyValue.BoolExpression (fn _ => let val  Var1 = Var1 ()
 val  Var2 = Var2 ()
 in (
(let val _ = if findB Var1 = NONE then badCh (Var1, Var1left, Var1right, "bool") else if findB Var2 = NONE then badCh (Var2, Var2left, Var2right, "bool") else () in OR (NO_OR (NO_AND (BFACT (BVAL (Bool Var1)))), (NO_AND (BFACT (BVAL (Bool Var2))))) end)
)
end)
 in ( LrTable.NT 16, ( result, Var1left, Var2right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.Boolterm Boolterm1, Boolterm1left, 
Boolterm1right)) :: rest671)) => let val  result = 
MlyValue.BoolExpression (fn _ => let val  (Boolterm as Boolterm1) = 
Boolterm1 ()
 in ((NO_OR Boolterm))
end)
 in ( LrTable.NT 16, ( result, Boolterm1left, Boolterm1right), rest671
)
end
|  ( 54, ( ( _, ( MlyValue.BoolFactor BoolFactor1, _, BoolFactor1right
)) :: _ :: ( _, ( MlyValue.Boolterm Boolterm1, Boolterm1left, _)) :: 
rest671)) => let val  result = MlyValue.Boolterm (fn _ => let val  (
Boolterm as Boolterm1) = Boolterm1 ()
 val  (BoolFactor as BoolFactor1) = BoolFactor1 ()
 in ((AND (Boolterm, BoolFactor)))
end)
 in ( LrTable.NT 17, ( result, Boolterm1left, BoolFactor1right), 
rest671)
end
|  ( 55, ( ( _, ( MlyValue.Var Var1, Varleft, (Varright as Var1right))
) :: _ :: ( _, ( MlyValue.Boolterm Boolterm1, Boolterm1left, _)) :: 
rest671)) => let val  result = MlyValue.Boolterm (fn _ => let val  (
Boolterm as Boolterm1) = Boolterm1 ()
 val  (Var as Var1) = Var1 ()
 in (
(let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in AND (Boolterm, (BVAL (Bool Var))) end)
)
end)
 in ( LrTable.NT 17, ( result, Boolterm1left, Var1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.BoolFactor BoolFactor1, _, BoolFactor1right
)) :: _ :: ( _, ( MlyValue.Var Var1, (Varleft as Var1left), Varright))
 :: rest671)) => let val  result = MlyValue.Boolterm (fn _ => let val 
 (Var as Var1) = Var1 ()
 val  (BoolFactor as BoolFactor1) = BoolFactor1 ()
 in (
(let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in AND ((NO_AND (BFACT (BVAL (Bool Var)))), BoolFactor) end)
)
end)
 in ( LrTable.NT 17, ( result, Var1left, BoolFactor1right), rest671)

end
|  ( 57, ( ( _, ( MlyValue.Var Var2, Var2left, Var2right)) :: _ :: ( _
, ( MlyValue.Var Var1, Var1left, Var1right)) :: rest671)) => let val  
result = MlyValue.Boolterm (fn _ => let val  Var1 = Var1 ()
 val  Var2 = Var2 ()
 in (
(let val _ = if findB Var1 = NONE then badCh (Var1, Var1left, Var1right, "bool") else if findB Var2 = NONE then badCh (Var2, Var2left, Var2right, "bool") else () in AND ((NO_AND (BFACT (BVAL (Bool Var1)))), (BVAL (Bool Var2))) end)
)
end)
 in ( LrTable.NT 17, ( result, Var1left, Var2right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.Comparison Comparison1, Comparison1left, 
Comparison1right)) :: rest671)) => let val  result = MlyValue.Boolterm
 (fn _ => let val  (Comparison as Comparison1) = Comparison1 ()
 in ((NO_AND Comparison))
end)
 in ( LrTable.NT 17, ( result, Comparison1left, Comparison1right), 
rest671)
end
|  ( 59, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.BoolFactor (fn _ => ((TT)))
 in ( LrTable.NT 18, ( result, TT1left, TT1right), rest671)
end
|  ( 60, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.BoolFactor (fn _ => ((FF)))
 in ( LrTable.NT 18, ( result, FF1left, FF1right), rest671)
end
|  ( 61, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.BoolExpression
 BoolExpression1, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) =>
 let val  result = MlyValue.BoolFactor (fn _ => let val  (
BoolExpression as BoolExpression1) = BoolExpression1 ()
 in ((BBRAC BoolExpression))
end)
 in ( LrTable.NT 18, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.Comparison Comparison1, _, Comparison1right
)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.BoolFactor (fn _ => let val  (Comparison as Comparison1) = 
Comparison1 ()
 in ((NOT Comparison))
end)
 in ( LrTable.NT 18, ( result, NOT1left, Comparison1right), rest671)

end
|  ( 63, ( ( _, ( MlyValue.Var Var1, Varleft, (Varright as Var1right))
) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.BoolFactor (fn _ => let val  (Var as Var1) = Var1 ()
 in (
(let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in NOT (BFACT (BVAL (Bool Var))) end)
)
end)
 in ( LrTable.NT 18, ( result, NOT1left, Var1right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: ( _, ( MlyValue.RelOP RelOP1, _, _)) :: ( _, 
( MlyValue.IntExpression IntExpression1, IntExpression1left, _)) :: 
rest671)) => let val  result = MlyValue.Comparison (fn _ => let val  
IntExpression1 = IntExpression1 ()
 val  (RelOP as RelOP1) = RelOP1 ()
 val  IntExpression2 = IntExpression2 ()
 in ((ICOMP (IntExpression1, RelOP, IntExpression2)))
end)
 in ( LrTable.NT 19, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 65, ( ( _, ( MlyValue.IntExpression IntExpression1, _, 
IntExpression1right)) :: ( _, ( MlyValue.RelOP RelOP1, _, _)) :: ( _, 
( MlyValue.Var Var1, (Varleft as Var1left), Varright)) :: rest671)) =>
 let val  result = MlyValue.Comparison (fn _ => let val  (Var as Var1)
 = Var1 ()
 val  (RelOP as RelOP1) = RelOP1 ()
 val  (IntExpression as IntExpression1) = IntExpression1 ()
 in (
(let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in ICOMP ((IEXP_sub (IEXP1_sub (IVAL (Int Var)))), RelOP, IntExpression) end)
)
end)
 in ( LrTable.NT 19, ( result, Var1left, IntExpression1right), rest671
)
end
|  ( 66, ( ( _, ( MlyValue.Var Var1, Varleft, (Varright as Var1right))
) :: ( _, ( MlyValue.RelOP RelOP1, _, _)) :: ( _, ( 
MlyValue.IntExpression IntExpression1, IntExpression1left, _)) :: 
rest671)) => let val  result = MlyValue.Comparison (fn _ => let val  (
IntExpression as IntExpression1) = IntExpression1 ()
 val  (RelOP as RelOP1) = RelOP1 ()
 val  (Var as Var1) = Var1 ()
 in (
(let val _ = if findI Var = NONE then badCh (Var, Varleft, Varright, "int") else () in ICOMP (IntExpression, RelOP, (IEXP_sub (IEXP1_sub (IVAL (Int Var))))) end)
)
end)
 in ( LrTable.NT 19, ( result, IntExpression1left, Var1right), rest671
)
end
|  ( 67, ( ( _, ( MlyValue.Var Var2, Var2left, Var2right)) :: ( _, ( 
MlyValue.RelOP RelOP1, _, _)) :: ( _, ( MlyValue.Var Var1, Var1left, 
Var1right)) :: rest671)) => let val  result = MlyValue.Comparison (fn
 _ => let val  Var1 = Var1 ()
 val  (RelOP as RelOP1) = RelOP1 ()
 val  Var2 = Var2 ()
 in (
(if findB Var1 = NONE andalso findI Var1 = NONE then let val _ = badCh (Var1, Var1left, Var1right, "any type") in (ICOMP ((IEXP_sub (IEXP1_sub (IVAL (Int Var1)))), RelOP, (IEXP_sub (IEXP1_sub (IVAL (Int Var2)))))) end else if findB Var1 = NONE then let val _ = if findI Var2 = NONE then badCh (Var2, Var2left, Var2right, "int") else () in (ICOMP ((IEXP_sub (IEXP1_sub (IVAL (Int Var1)))), RelOP, (IEXP_sub (IEXP1_sub (IVAL (Int Var2)))))) end else let val _ = if findB Var2 = NONE then badCh (Var2, Var2left, Var2right, "bool") else () in  ((BCOMP ((BFACT (BVAL (Bool Var1))), RelOP, (BVAL (Bool Var2))))) end)
)
end)
 in ( LrTable.NT 19, ( result, Var1left, Var2right), rest671)
end
|  ( 68, ( ( _, ( MlyValue.BoolFactor BoolFactor1, _, BoolFactor1right
)) :: ( _, ( MlyValue.RelOP RelOP1, _, _)) :: ( _, ( 
MlyValue.Comparison Comparison1, Comparison1left, _)) :: rest671)) =>
 let val  result = MlyValue.Comparison (fn _ => let val  (Comparison
 as Comparison1) = Comparison1 ()
 val  (RelOP as RelOP1) = RelOP1 ()
 val  (BoolFactor as BoolFactor1) = BoolFactor1 ()
 in ((BCOMP (Comparison, RelOP, BoolFactor)))
end)
 in ( LrTable.NT 19, ( result, Comparison1left, BoolFactor1right), 
rest671)
end
|  ( 69, ( ( _, ( MlyValue.Var Var1, Varleft, (Varright as Var1right))
) :: ( _, ( MlyValue.RelOP RelOP1, _, _)) :: ( _, ( 
MlyValue.Comparison Comparison1, Comparison1left, _)) :: rest671)) =>
 let val  result = MlyValue.Comparison (fn _ => let val  (Comparison
 as Comparison1) = Comparison1 ()
 val  (RelOP as RelOP1) = RelOP1 ()
 val  (Var as Var1) = Var1 ()
 in (
(let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in BCOMP (Comparison, RelOP, (BVAL (Bool Var))) end)
)
end)
 in ( LrTable.NT 19, ( result, Comparison1left, Var1right), rest671)

end
|  ( 70, ( ( _, ( MlyValue.BoolFactor BoolFactor1, _, BoolFactor1right
)) :: ( _, ( MlyValue.RelOP RelOP1, _, _)) :: ( _, ( MlyValue.Var Var1
, (Varleft as Var1left), Varright)) :: rest671)) => let val  result = 
MlyValue.Comparison (fn _ => let val  (Var as Var1) = Var1 ()
 val  (RelOP as RelOP1) = RelOP1 ()
 val  (BoolFactor as BoolFactor1) = BoolFactor1 ()
 in (
(let val _ = if findB Var = NONE then badCh (Var, Varleft, Varright, "bool") else () in BCOMP ((BFACT (BVAL (Bool Var))), RelOP, BoolFactor) end)
)
end)
 in ( LrTable.NT 19, ( result, Var1left, BoolFactor1right), rest671)

end
|  ( 71, ( ( _, ( MlyValue.BoolFactor BoolFactor1, BoolFactor1left, 
BoolFactor1right)) :: rest671)) => let val  result = 
MlyValue.Comparison (fn _ => let val  (BoolFactor as BoolFactor1) = 
BoolFactor1 ()
 in ((BFACT BoolFactor))
end)
 in ( LrTable.NT 19, ( result, BoolFactor1left, BoolFactor1right), 
rest671)
end
|  ( 72, ( ( _, ( _, LT1left, LT1right)) :: rest671)) => let val  
result = MlyValue.RelOP (fn _ => ((LT)))
 in ( LrTable.NT 20, ( result, LT1left, LT1right), rest671)
end
|  ( 73, ( ( _, ( _, GT1left, GT1right)) :: rest671)) => let val  
result = MlyValue.RelOP (fn _ => ((GT)))
 in ( LrTable.NT 20, ( result, GT1left, GT1right), rest671)
end
|  ( 74, ( ( _, ( _, LEQ1left, LEQ1right)) :: rest671)) => let val  
result = MlyValue.RelOP (fn _ => ((LEQ)))
 in ( LrTable.NT 20, ( result, LEQ1left, LEQ1right), rest671)
end
|  ( 75, ( ( _, ( _, GEQ1left, GEQ1right)) :: rest671)) => let val  
result = MlyValue.RelOP (fn _ => ((GEQ)))
 in ( LrTable.NT 20, ( result, GEQ1left, GEQ1right), rest671)
end
|  ( 76, ( ( _, ( _, NEQ1left, NEQ1right)) :: rest671)) => let val  
result = MlyValue.RelOP (fn _ => ((NEQ)))
 in ( LrTable.NT 20, ( result, NEQ1left, NEQ1right), rest671)
end
|  ( 77, ( ( _, ( _, EQ1left, EQ1right)) :: rest671)) => let val  
result = MlyValue.RelOP (fn _ => ((EQ)))
 in ( LrTable.NT 20, ( result, EQ1left, EQ1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.begin x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : WHILE_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun CARET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DVBAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun IDE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.IDE (fn () => i),p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun ILLCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun INPUT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun NEW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun PRO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LFLO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun RFLO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun DOUBLECOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun NUMBER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.NUMBER (fn () => i),p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun WHI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
end
end
