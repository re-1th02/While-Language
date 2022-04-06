structure SymbolTable :
    sig val find : string -> (int * DataTypes.Type) option
        val add : (string * (int * DataTypes.Type)) -> unit
        val nullify : unit -> unit;
        end = struct
            fun exit msg = let val _ = print(msg^"\n") in OS.Process.exit(OS.Process.success) end;
            fun f (s:string) ((key,v)::r) = if s=key then SOME v else f s r | f s nil = NONE
            val TableSize   = 422 (* 211 *)
            val HashFactor  = 5
            val hash        = fn s  => List.foldr (fn (c,v) => (v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
            val HashTable   = ref (Array.array(TableSize,nil)) : ((string * (int * DataTypes.Type)) list Array.array) ref
            val add         = fn (s,v) =>   let val i = hash s; 
                                            in 
                                            case (f s (Array.sub(!HashTable, i))) of
                                            NONE => Array.update(!HashTable,i,(s,v) :: (Array.sub(!HashTable, i)))
                                            | SOME v => exit ("\nERROR : Variable "^s^" is declared more than once!!") end
            val find        = fn s => let  val i = hash s in f s (Array.sub(!HashTable, i)) end
            val nullify     = fn ()=> HashTable := Array.array(TableSize,nil);
end;

structure PositionTable :
    sig val find : string -> (int * int) option
        val add : (string * (int * int)) -> unit
        val nullify : unit -> unit;
        end = struct
            fun exit msg = let val _ = print(msg^"\n") in OS.Process.exit(OS.Process.success) end;
            fun f (s:string) ((key,v)::r) = if s=key then SOME v else f s r | f s nil = NONE
            val TableSize   = 422 (* 211 *)
            val HashFactor  = 5
            val hash        = fn s  => List.foldr (fn (c,v) => (v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
            val HashTable   = ref (Array.array(TableSize,nil)) : ((string * (int * int)) list Array.array) ref
            val add         = fn (s,v) =>   let val i = hash s; 
                                            in 
                                            case (f s (Array.sub(!HashTable, i))) of
                                            NONE => Array.update(!HashTable,i,(s,v) :: (Array.sub(!HashTable, i)))
                                            | SOME v => () end
            val find        = fn s => let  val i = hash s in f s (Array.sub(!HashTable, i)) end
            val nullify     = fn ()=> HashTable := Array.array(TableSize,nil);
end;