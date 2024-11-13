val _ = print "~~~~~~~~ isolate ~~~~~~~~\n";
val test_type: ''a list -> ''a list = isolate;
val test = isolate [1,2,34,6,7,4,5,5,3,4,5,6,2,1,3,0] = [1,2,34,6,7,4,5,3,0];

val _ = print "~~~~~~~~ getVars ~~~~~~~~\n";
val test_type: ''a expression -> ''a list = getVars;
val test = getVars (Var "A") = ["A"];
val test = getVars (Eq [Var "A", Var "B", Imp (Var "D", Not (Var "Q")), Var "D", Var "B"]) = ["A","B","D","Q"];

val _ = print "~~~~~~~~ eval ~~~~~~~~\n";
val test_type: ''a list -> ''a expression -> bool = eval;
val test = eval [2, 3] (And [True, Or [Var 1, Not (Not (Var 2))], Imp (Var 1, Var 2)]) = true;
val test = eval [] (Eq [Var 1, False, False, True]) = false;
