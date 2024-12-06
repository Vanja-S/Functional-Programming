(* izpis daljših izrazov v interpreterju *)
val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;


val _ = print "~~~~~~~~ Rational.makeRational ~~~~~~~~\n";
val test_type: int * int -> Rational.rational = Rational.makeRational;
val test = Rational.makeRational (3,4);
print(Rational.toString test);
print("\n");
val test = (Rational.makeRational (3, 0); false) 
           handle BadRational => true;
val test = Rational.makeRational (7,7);
print(Rational.toString test);
print("\n");