structure Vmc :
    sig val rules : (int FunStack.Stack * (int option) Array.array * DataTypes.UniversalDT FunStack.Stack) -> (int FunStack.Stack * (int option) Array.array * DataTypes.UniversalDT FunStack.Stack)
        val toString : (int FunStack.Stack * (int option) Array.array * DataTypes.UniversalDT FunStack.Stack) -> (string * string * string)
        end = struct

        fun pushlist [] l = l
        |   pushlist (x::xs) l = (FunStack.push((DataTypes.cmd x), (pushlist xs l)));

        fun exit msg = let val _ = print(msg^"\n") in OS.Process.exit(OS.Process.success) end;

        fun inp s = let val i = valOf (TextIO.inputLine TextIO.stdIn)in
            case (SymbolTable.find s) of
                SOME (x, DataTypes.BOOL) => if i = "tt\n" then (DataTypes.SET_BOOL ((DataTypes.Bool s), (DataTypes.NO_OR (DataTypes.NO_AND (DataTypes.BFACT (DataTypes.TT))))))
                                    else if i = "ff\n" then (DataTypes.SET_BOOL ((DataTypes.Bool s), (DataTypes.NO_OR (DataTypes.NO_AND (DataTypes.BFACT (DataTypes.FF))))))
                                    else exit "Error!! You need to enter a bool value(i.e., tt or ff)"
            |   SOME (x, DataTypes.INT) => let val j = (Int.fromString i) in 
                                    (case j of 
                                        SOME v => (DataTypes.SET_INT (DataTypes.Int s, (DataTypes.IEXP_sub (DataTypes.IEXP1_sub (DataTypes.NUM v)))))
                                    |   NONE   => exit "Error!! You need to enter a integer value")
                                    end
            |   _                      => exit "Unknown Error while reading the input!!"
            end

        fun push_postfix (DataTypes.cmd        (DataTypes.SET_INT ((DataTypes.Int x), y)))             l   = FunStack.push((DataTypes.str x), FunStack.push((DataTypes.intexp y), FunStack.push(DataTypes.SET, l)))
        |   push_postfix (DataTypes.cmd        (DataTypes.SET_BOOL ((DataTypes.Bool x), y)))           l   = FunStack.push((DataTypes.str x), FunStack.push((DataTypes.boolexp y), FunStack.push(DataTypes.SET, l)))
        |   push_postfix (DataTypes.cmd        (DataTypes.READ (DataTypes.Int x)))                     l   = FunStack.push((DataTypes.str x), FunStack.push(DataTypes.read, l))
        |   push_postfix (DataTypes.cmd        (DataTypes.READ (DataTypes.Bool x)))                    l   = FunStack.push((DataTypes.str x), FunStack.push(DataTypes.read, l))
        |   push_postfix (DataTypes.cmd        (DataTypes.WRITEI y))                                   l   = FunStack.push((DataTypes.intexp y), FunStack.push(DataTypes.WRITE, l))
        |   push_postfix (DataTypes.cmd        (DataTypes.WRITEB y))                                   l   = FunStack.push((DataTypes.boolexp y), FunStack.push(DataTypes.WRITE, l))
        |   push_postfix (DataTypes.cmd        (DataTypes.ITE (e, DataTypes.SEQ c1,DataTypes.SEQ c2))) l   = FunStack.push((DataTypes.boolexp e), (FunStack.push((DataTypes.cmdlist c1), (FunStack.push((DataTypes.cmdlist c2), FunStack.push(DataTypes.ite, l))) ) ))   
        |   push_postfix (DataTypes.cmd        (DataTypes.WH (e, DataTypes.SEQ c)))                    l   = FunStack.push((DataTypes.boolexp e), FunStack.push((DataTypes.cmdlist c), FunStack.push((DataTypes.wh), (FunStack.push((DataTypes.cmd (DataTypes.WH (e, DataTypes.SEQ c))), l)))))
        |   push_postfix (DataTypes.intexp     ((DataTypes.IEXP(i, k, n))))                            l   = (push_postfix ((DataTypes.intexp) i) (push_postfix ((DataTypes.noaddexp) n) (FunStack.push((DataTypes.oop ("", k)), l))))        (*Edit*)
        |   push_postfix (DataTypes.intexp     (DataTypes.IEXP_sub n))                                 l   = (push_postfix ((DataTypes.noaddexp) n) l)
        |   push_postfix (DataTypes.noaddexp   (DataTypes.IEXP1(a, k, m)))                             l   = (push_postfix ((DataTypes.noaddexp) a) (push_postfix (DataTypes.nomultexp m) (FunStack.push((DataTypes.oop ("", k)), l))))  (*Edit*)
        |   push_postfix (DataTypes.noaddexp   (DataTypes.IEXP1_sub m))                                l   = (push_postfix ((DataTypes.nomultexp) m) l)
        |   push_postfix (DataTypes.nomultexp  (DataTypes.IBRAC i))                                    l   = (push_postfix (DataTypes.intexp i) l)
        |   push_postfix (DataTypes.nomultexp  (DataTypes.NUM x))                                      l   = FunStack.push((DataTypes.num x), l)
        |   push_postfix (DataTypes.nomultexp  (DataTypes.IVAL (DataTypes.Int x)))                     l   = FunStack.push((DataTypes.str x), l)
        |   push_postfix (DataTypes.nomultexp  (DataTypes.NEG m))                                      l   = FunStack.push((DataTypes.num 0), push_postfix (DataTypes.nomultexp m) (FunStack.push((DataTypes.oop ("", DataTypes.MINUS)), l)))      (*Edit*)
        |   push_postfix (DataTypes.boolexp    (DataTypes.OR (e, t)))                                  l   = (push_postfix (DataTypes.boolexp e) (push_postfix (DataTypes.bterm t) (FunStack.push(DataTypes.or, l))))
        |   push_postfix (DataTypes.boolexp    (DataTypes.NO_OR t))                                    l   = (push_postfix (DataTypes.bterm t) l)
        |   push_postfix (DataTypes.bterm      (DataTypes.AND (t, f)))                                 l   = (push_postfix (DataTypes.bterm t) (push_postfix (DataTypes.bfact f) (FunStack.push(DataTypes.nd, l))))
        |   push_postfix (DataTypes.bterm      (DataTypes.NO_AND c))                                   l   = (push_postfix (DataTypes.cmp c) l)
        |   push_postfix (DataTypes.cmp        (DataTypes.ICOMP (i1, k, i2)))                          l   = (push_postfix (DataTypes.intexp i1) (push_postfix (DataTypes.intexp i2) (FunStack.push((DataTypes.reloop ("", k)), l))))      (*Edit*)
        |   push_postfix (DataTypes.cmp        (DataTypes.BCOMP (c, k, f)))                            l   = (push_postfix (DataTypes.cmp c) (push_postfix (DataTypes.bfact f) (FunStack.push((DataTypes.reloop ("", k)), l))))              (*Edit*)
        |   push_postfix (DataTypes.cmp        (DataTypes.BFACT f))                                    l   = (push_postfix (DataTypes.bfact f) l)
        |   push_postfix (DataTypes.bfact      (DataTypes.BBRAC e))                                    l   = (push_postfix (DataTypes.boolexp e) l)
        |   push_postfix (DataTypes.bfact      (DataTypes.NOT c))                                      l   = (push_postfix ((DataTypes.cmp) c) (FunStack.push(DataTypes.not, l)))
        |   push_postfix (DataTypes.bfact       DataTypes.TT)                                          l   = FunStack.push((DataTypes.num 1), l)
        |   push_postfix (DataTypes.bfact       DataTypes.FF)                                          l   = FunStack.push((DataTypes.num 0), l)
        |   push_postfix (DataTypes.bfact      (DataTypes.BVAL (DataTypes.Bool x)))                    l   = FunStack.push((DataTypes.str x), l)
        |   push_postfix _ l = let val _ = exit "Unknown Structure Error!!" in l end
          
        fun toStringopI DataTypes.PLUS = "PLUS"
        |   toStringopI DataTypes.MINUS= "MINUS"
        |   toStringopI DataTypes.TIMES= "TIMES"
        |   toStringopI DataTypes.DIV= "DIV"
        |   toStringopI DataTypes.MOD= "MOD"

        fun toStringopB DataTypes.LT= "LT"
        |   toStringopB DataTypes.GT= "GT"
        |   toStringopB DataTypes.LEQ= "LEQ"
        |   toStringopB DataTypes.GEQ= "GEQ"
        |   toStringopB DataTypes.EQ= "EQ"
        |   toStringopB DataTypes.NEQ= "NEQ" 

        fun toStringUT (DataTypes.cmd        (DataTypes.SET_INT ((DataTypes.Int x), y)))            = "("^x^" "^toStringUT(DataTypes.intexp y)^" SET)"
        |   toStringUT (DataTypes.cmd        (DataTypes.SET_BOOL ((DataTypes.Bool x), y)))          = "("^x^" "^toStringUT(DataTypes.boolexp y)^" SET)"
        |   toStringUT (DataTypes.cmd        (DataTypes.READ (DataTypes.Int x)))                    = "("^x^" read)"
        |   toStringUT (DataTypes.cmd        (DataTypes.READ (DataTypes.Bool x)))                    = "("^x^" read)"
        |   toStringUT (DataTypes.cmd        (DataTypes.WRITEI y))                                  = "("^(toStringUT (DataTypes.intexp y))^" write"
        |   toStringUT (DataTypes.cmd        (DataTypes.WRITEB y))                                  = "("^(toStringUT (DataTypes.boolexp y))^" write"
        |   toStringUT (DataTypes.cmd        (DataTypes.ITE (e, DataTypes.SEQ c1,DataTypes.SEQ c2)))= let fun f [] = "" | f (x::xs) = (toStringUT (DataTypes.cmd x))^" "^(f xs) in "("^(toStringUT (DataTypes.boolexp e))^" ["^(f c1)^"] ["^(f c2)^"])" end
        |   toStringUT (DataTypes.cmd        (DataTypes.WH (e, DataTypes.SEQ c)))                   = let fun f [] = "" | f (x::xs) = (toStringUT (DataTypes.cmd x))^" "^(f xs) in "("^(toStringUT (DataTypes.boolexp e))^" ["^(f c)^"])" end
        |   toStringUT (DataTypes.intexp     ((DataTypes.IEXP(i, k, n))))                           = "("^(toStringUT (DataTypes.intexp i))^" "^(toStringUT (DataTypes.noaddexp n))^" "^(toStringopI k)^")"
        |   toStringUT (DataTypes.intexp     (DataTypes.IEXP_sub n))                                = "("^(toStringUT (DataTypes.noaddexp n))^")"
        |   toStringUT (DataTypes.noaddexp   (DataTypes.IEXP1(a, k, m)))                            = (toStringUT (DataTypes.noaddexp a))^" "^(toStringUT (DataTypes.nomultexp m))^" "^(toStringopI k)
        |   toStringUT (DataTypes.noaddexp   (DataTypes.IEXP1_sub m))                               = (toStringUT (DataTypes.nomultexp m))
        |   toStringUT (DataTypes.nomultexp  (DataTypes.IBRAC i))                                   = (toStringUT (DataTypes.intexp i))
        |   toStringUT (DataTypes.nomultexp  (DataTypes.NUM x))                                     = Int.toString(x)
        |   toStringUT (DataTypes.nomultexp  (DataTypes.IVAL (DataTypes.Int x)))                    = x
        |   toStringUT (DataTypes.nomultexp  (DataTypes.NEG m))                                     = "0 "^(toStringUT (DataTypes.nomultexp m))^" MINUS"
        |   toStringUT (DataTypes.boolexp    (DataTypes.OR (e, t)))                                 = "("^(toStringUT (DataTypes.boolexp e)) ^ " " ^ (toStringUT (DataTypes.bterm t)) ^ " OR)"
        |   toStringUT (DataTypes.boolexp    (DataTypes.NO_OR t))                                   = "("^(toStringUT (DataTypes.bterm t))^ ")"
        |   toStringUT (DataTypes.bterm      (DataTypes.AND (t, f)))                                = (toStringUT (DataTypes.bterm t))^" "^(toStringUT (DataTypes.bfact f))^ " AND)"
        |   toStringUT (DataTypes.bterm      (DataTypes.NO_AND c))                                  = (toStringUT (DataTypes.cmp c))
        |   toStringUT (DataTypes.cmp        (DataTypes.ICOMP (i1, k, i2)))                         = (toStringUT (DataTypes.intexp i1))^" "^(toStringUT (DataTypes.intexp i2))^" "^(toStringopB k)
        |   toStringUT (DataTypes.cmp        (DataTypes.BCOMP (c, k, f)))                           = (toStringUT (DataTypes.cmp c))^" "^(toStringUT (DataTypes.bfact f))^" "^(toStringopB k)
        |   toStringUT (DataTypes.cmp        (DataTypes.BFACT f))                                   = (toStringUT (DataTypes.bfact f))
        |   toStringUT (DataTypes.bfact      (DataTypes.BBRAC e))                                   = (toStringUT (DataTypes.boolexp e))
        |   toStringUT (DataTypes.bfact      (DataTypes.NOT c))                                     = (toStringUT (DataTypes.cmp c))^" NOT"
        |   toStringUT (DataTypes.bfact       DataTypes.TT)                                         = "tt"
        |   toStringUT (DataTypes.bfact       DataTypes.FF)                                         = "ff"
        |   toStringUT (DataTypes.bfact      (DataTypes.BVAL (DataTypes.Bool x)))                   = x
        |   toStringUT (DataTypes.cmdlist    l)                                                     = let fun f [] = "" | f (x::xs) = (toStringUT (DataTypes.cmd x))^" "^(f xs) in "(["^(f l)^"])" end
        |   toStringUT (DataTypes.num x)                                                            = Int.toString(x)
        |   toStringUT (DataTypes.str x)                                                            = x
        |   toStringUT DataTypes.SET                                                                = "SET"
        |   toStringUT DataTypes.ite                                                                = "ITE"
        |   toStringUT DataTypes.wh                                                                 = "WH"
        |   toStringUT DataTypes.read                                                               = "READ"
        |   toStringUT DataTypes.WRITE                                                              = "WRITE"
        |   toStringUT (DataTypes.oop (x, k))                                                       = (toStringopI k)
        |   toStringUT (DataTypes.reloop (x, k))                                                    = (toStringopB k)
        |   toStringUT DataTypes.or                                                                 = "OR"
        |   toStringUT DataTypes.nd                                                                 = "AND"
        |   toStringUT DataTypes.not                                                                = "NOT"
        |   toStringUT _                                                                            = "#"

        fun toStringM l = let
            fun toStringI l i = if Array.length(l) = i then "" else 
                                case Array.sub(l, i) of
                                SOME v => if i = 0 then (Int.toString(v)^(toStringI l (i+1))) else (", "^Int.toString(v)^(toStringI l (i+1)))
                            |   NONE   => if i = 0 then ("#"^(toStringI l (i+1))) else (", #"^(toStringI l (i+1)))
            in "["^(toStringI l 0)^"]" end;

        fun toString (V, M, C) = 
          ((FunStack.toString (Int.toString) V), (toStringM M), (FunStack.toString (toStringUT) C))

        fun findVal Memory s = let val (SOME (x,y)) = (SymbolTable.find s) in
                         (case Array.sub(Memory, x) of SOME v => v | NONE => let val SOME (s1, s2) = PositionTable.find(s) in exit ("Error!! Variable "^s^" has used before initialising it with a value   "^"Line : "^Int.toString(s1)^" Col : "^Int.toString(s2)) end) end

        fun operateI(m, n, DataTypes.PLUS) = m+n
        |   operateI(m, n, DataTypes.MINUS)= m-n
        |   operateI(m, n, DataTypes.TIMES)= m*n
        |   operateI(m, n, DataTypes.DIV)  = m div n
        |   operateI(m, n, DataTypes.MOD)  = m mod n

        fun operateB(m, n, DataTypes.LT)   = if (m<n)  then 1 else 0
        |   operateB(m, n, DataTypes.GT)   = if (m>n)  then 1 else 0
        |   operateB(m, n, DataTypes.GEQ)  = if (m>=n) then 1 else 0
        |   operateB(m, n, DataTypes.LEQ)  = if (m<=n) then 1 else 0
        |   operateB(m, n, DataTypes.EQ)   = if (m=n)  then 1 else 0
        |   operateB(m, n, DataTypes.NEQ)  = if (m=n)  then 0 else 1

        fun opOr(m, n) = if (m+n > 0) then 1 else 0
        fun opAnd(m, n) = m*n
        fun opNot(m) = 1-m

        fun rules(V, M, EMPTY) = let val _ = SymbolTable.nullify () and _ = PositionTable.nullify () in (V, M, EMPTY) end
        |   rules((pile(1, V)),              M, (pile((DataTypes.cmdlist c), (pile((DataTypes.cmdlist d), (pile((DataTypes.ite), C)))))))= rules(V, M, (pushlist c C))
        |   rules((pile(0, V)),              M, (pile((DataTypes.cmdlist c), (pile((DataTypes.cmdlist d), (pile((DataTypes.ite), C)))))))= rules(V, M, (pushlist d C))
        |   rules((pile(0, V)),              M, (pile((DataTypes.cmdlist c), (pile((DataTypes.wh), C)))))                                = rules(V, M, FunStack.pop(C))
        |   rules((pile(1, V)),              M, (pile((DataTypes.cmdlist c), (pile((DataTypes.wh), C)))))                                = rules(V, M, (pushlist c C))
        |   rules((pile(x, V)),              M, (pile(DataTypes.WRITE, C)))                                                              = let val _ = print(Int.toString(x)^"\n") in rules(V, M, C) end
        |   rules((V,                        M, (pile((DataTypes.str x), pile((DataTypes.read), C)))))                                   = rules((V, M, (pile((DataTypes.cmd (inp x)), C))))
        |   rules( V,                        M, (pile((DataTypes.num m), C)))                                                            = rules(FunStack.push(m, V), M, C)
        |   rules( V,                        M, (pile((DataTypes.str x), (pile((DataTypes.intexp e), pile(DataTypes.SET, C))))))         = let val SOME (i, j) = (SymbolTable.find x) in rules(FunStack.push(i, V), M, (pile((DataTypes.intexp e), pile(DataTypes.SET, C)))) end
        |   rules( V,                        M, (pile((DataTypes.str x), (pile((DataTypes.boolexp e), pile(DataTypes.SET, C))))))        = let val SOME (i, j) = (SymbolTable.find x) in rules(FunStack.push(i, V), M, (pile((DataTypes.boolexp e), pile(DataTypes.SET, C)))) end
        |   rules( V,                        M, (pile((DataTypes.str x), C)))                                                            = rules(FunStack.push((findVal M x), V), M, C)
        |   rules((pile (n, pile(m, V))),    M, (pile((DataTypes.oop ("", k)), C)))                                                      = rules(FunStack.push((operateI(m, n, k)), V), M, C)
        |   rules((pile (n, pile(m, V))),    M, (pile((DataTypes.reloop ("", k)), C)))                                                   = rules(FunStack.push((operateB(m, n, k)), V), M, C)
        |   rules((pile (n, pile(m, V))),    M, (pile((DataTypes.or), C)))                                                               = rules(FunStack.push((opOr(m, n)), V), M, C)
        |   rules((pile (n, pile(m, V))),    M, (pile((DataTypes.nd), C)))                                                               = rules(FunStack.push((opAnd(m, n)), V), M, C)
        |   rules((pile (n, V)),             M, (pile((DataTypes.not), C)))                                                              = rules(FunStack.push((opNot(n)), V), M, C)
        |   rules( V,                        M, (pile(DataTypes.cmd x, C)))                                                              = rules(V, M, (push_postfix (DataTypes.cmd x) C))
        |   rules( V,                        M, (pile(DataTypes.intexp x, C)))                                                           = rules(V, M, (push_postfix (DataTypes.intexp x) C))
        |   rules( V,                        M, (pile(DataTypes.boolexp x, C)))                                                          = rules(V, M, (push_postfix (DataTypes.boolexp x) C))
        |   rules((pile(m, pile(x, V))),     M, (pile(DataTypes.SET, C)))                                                                = let val _ = Array.update(M, x, (SOME m)) in rules(V, M, C) end
        |   rules(V, M, C) = let val _ = exit "Unknown Structure Error!!" in (V, M, C) end

            
end;

structure WHILE :
sig val compile : string -> DataTypes.Program
    val execute    : string -> (int FunStack.Stack * (int option) Array.array * DataTypes.UniversalDT FunStack.Stack)
end = struct exception WHILEError;
fun exit msg = let val _ = print(msg^"\n") in OS.Process.exit(OS.Process.success) end;
fun pushlist [] l = l
    |   pushlist (x::xs) l = (FunStack.push((DataTypes.cmd x), (pushlist xs l)));

fun compile (fileName) =
    let val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn n =>  if TextIO.endOfStream inStream
                                            then ""
                                            else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn (msg,line,col) => if line = col then exit (fileName^"["^Int.toString line^"] "^"Syntax error"^"\n") else exit (fileName^"["^Int.toString line^":"^Int.toString col^"] "^"Syntax error"^"\n");
        val (tree,rem) = WHILEParser.parse(15,(WHILEParser.makeLexer grab fileName),printError,fileName)
        handle WHILEParser.ParseError => raise WHILEError;
        val _ = TextIO.closeIn inStream;
        val DataTypes.PROG (a, b) = tree;
    in 
        tree
    end

    fun execute file = 
    let
        val DataTypes.PROG (a, b) = compile file;
        fun extract_cmdseq (DataTypes.BLK_DEC (c, DataTypes.SEQ d)) = (c, d) 
        |   extract_cmdseq (DataTypes.BLK_NODEC (DataTypes.SEQ d)) = (DataTypes.DEC [], d);
        val (DataTypes.DEC l, d) = extract_cmdseq b;
        fun push ([], b) l i = (l, i)
        |   push ((x::xs), b) l i = push (xs, b) ((x,(i, b))::l) (i+1);
        fun assign [] l i = (l, i)
        |   assign ((a, b)::xs) l i= let val (x, y) = (push (a, b) l i) in (assign xs x y) end;
        val (lst, memLength) = (assign l [] 0);
        val _ = List.app SymbolTable.add lst;
        val x = Vmc.rules(FunStack.create, (Array.array(memLength, NONE) : (int option) Array.array), (pushlist d FunStack.create))
    in
        x
    end;
end;
