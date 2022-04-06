datatype 'a stack = EMPTY | pile of 'a * 'a stack

signature STACK =
    sig
    exception EmptyStack
    exception Error of string
    type 'a Stack
    val create      : 'a Stack
    val push        : 'a * 'a Stack -> 'a Stack
    val pop         : 'a Stack -> 'a Stack
    val top         : 'a Stack -> 'a
    val empty       : 'a Stack -> bool
    val poptop      : 'a Stack -> ('a * 'a Stack) option
    val nth         : 'a Stack * int -> 'a
    val drop        : 'a Stack * int -> 'a Stack
    val depth       : 'a Stack -> int
    val app         : ('a -> unit) -> 'a Stack -> unit
    val map         : ('a -> 'b) -> 'a Stack -> 'b Stack
    val mapPartial  : ('a -> 'b option) -> 'a Stack -> 'b Stack
    val find        : ('a -> bool) -> 'a Stack -> 'a option
    val filter      : ('a -> bool) -> 'a Stack -> 'a Stack
    val foldr       : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val foldl       : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val exists      : ('a -> bool) -> 'a Stack -> bool
    val all         : ('a -> bool) -> 'a Stack -> bool
    val list2stack  : 'a list ->'a Stack 
    val stack2list  : 'a Stack -> 'a list 
    val toString    : ('a -> string) -> 'a Stack -> string
end;

structure FunStack : STACK =
struct
    type 'a Stack = 'a stack
    exception EmptyStack
    exception Error of string
    val create     = EMPTY;
    fun push    (a, b) = pile (a, b);
    fun pop EMPTY = raise EmptyStack
    |   pop (pile(x, y)) = y;
    fun top EMPTY = raise EmptyStack
    |   top (pile(x, y)) = x;
    fun empty EMPTY = true
    |   empty x     = false;
    fun poptop EMPTY = NONE
    |   poptop (pile(x, y)) = SOME (x, y);
    fun nth (EMPTY, n) = if n<1 then raise Error "cannot access element with index <1 from stack!!" else raise Error "Index is greater than Length!!"
    |   nth (pile(x, y), 1)= x
    |   nth (pile(x, y), n)= if n<1 then raise Error "cannot access element with index <1 from stack!!" else nth (y, n-1)
    fun drop (a, 0) = a
    |   drop (EMPTY, n) = if n>0 then raise Error "Index is greater than Length!!" else raise Error "number of elements to be dropped should be greater than zero"
    |   drop (pile(x, y), n)= if n<0 then raise Error "number of elements to be dropped should be greater than zero" else (drop (y, (n-1)))
    fun depth EMPTY = 0
    |   depth (pile(x, y)) = 1+ (depth y)
    fun app f EMPTY = ()
    |   app f (pile(x, y)) = let val i = (app f y) in (f x) end;
    fun map f EMPTY = EMPTY
    |   map f (pile(x, y)) = pile((f x), (map f y))
    fun mapPartial f EMPTY = EMPTY
    |   mapPartial f (pile(x, y)) = case (f x) of NONE => (mapPartial f y) | SOME v => (pile(v, (mapPartial f y)))
    fun find f EMPTY = NONE
    |   find f (pile(x, y)) = case (f x) of true => SOME (x) | false => (find f y) 
    fun filter f EMPTY = EMPTY
    |   filter f (pile(x, y)) = case (f x) of true => pile(x, (filter f y)) | false => (filter f y)
    fun foldr f i EMPTY = i
    |   foldr f i (pile(x, y)) = f (x, (foldr f i y))
    fun foldl f i EMPTY = i
    |   foldl f i (pile(x, y)) = foldl f (f(x, i)) y
    fun exists f EMPTY = false
    |   exists f (pile(x, y)) = case (f x) of true => true | false => (exists f y)
    fun all f EMPTY = true
    |   all f (pile(x, y)) = case (f x) of false => false | true => (all f y)
    fun list2stack [] = EMPTY
    |   list2stack (x::xs) = pile(x, (list2stack xs))
    fun stack2list EMPTY = []
    |   stack2list (pile(x, y)) = x::(stack2list y)
    fun toString f l = let fun g EMPTY = "" | g (pile(x, EMPTY)) = f(x) | g(pile(x, y)) = f(x) ^ ", " ^ (g y) in "["^(g l)^"]" end
end;