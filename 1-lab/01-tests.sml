val _ = print "~~~~~~~~ factorial ~~~~~~~~\n";
val test_type: int -> int = factorial;
val test = factorial (6) = 720;

val _ = print "~~~~~~~~ power ~~~~~~~~\n";
val test_type: int * int -> int = power;
val test = power (2, 10) = 1024;
val test = power (2, 5) = 32;

val _ = print "~~~~~~~~ gcd ~~~~~~~~\n";
val test_type: int * int -> int = gcd;
val test = gcd (60, 48) = 12;
val test = gcd (15, 3) = 3;

val _ = print "~~~~~~~~ len ~~~~~~~~\n";
val test_type: int list -> int = len;
val test = len ([1, 2 ,3, 4, 5]) = 5;

val _ = print "~~~~~~~~ last ~~~~~~~~\n";
val test_type: int list -> int option = last;
val test = valOf (last ([1, 2 ,3, 4, 5])) = 5;
val test = valOf (last ([1, 2 ,3, 4])) = 4;
val test = valOf (last ([10, 864, 19, 1])) = 1;
val test = isSome (last ([])) = false;

val _ = print "~~~~~~~~ nth ~~~~~~~~\n";
val test_type: int list * int -> int option = nth;
val test = valOf (nth ([1, 2, 3, 4, 5], 4)) = 5;
val test = valOf (nth ([1, 2, 3, 4, 5], 0)) = 1;
val test = isSome (nth ([1, 2, 3, 4, 5], 10)) = false;

val _ = print "~~~~~~~~ insert ~~~~~~~~\n";
val test_type: int list * int * int -> int list = insert;
val test = insert ([1, 2, 3], 0, 10) = [10, 1, 2, 3];
val test = insert ([1, 2, 3], 3, 10) = [1, 2, 3, 10];

val _ = print "~~~~~~~~ delete ~~~~~~~~\n";
val test_type: int list * int -> int list = delete;
val test = delete ([1, 2, 3], 1) = [2, 3];
val test = delete ([], 1) = [];
val test = delete ([1, 2, 2, 3, 2], 2) = [1,3];

val _ = print "~~~~~~~~ reverse ~~~~~~~~\n";
val test_type: int list -> int list = reverse;
val test = reverse ([1, 2, 3]) = [3, 2, 1];
val test = reverse ([]) = [];

val _ = print "~~~~~~~~ palindrome ~~~~~~~~\n";
val test_type: int list -> bool = palindrome;
val test = palindrome [1, 2, 3, 2, 1] = true;
val test = palindrome [1, 1, 1, 1] = true;
val test = palindrome [1, 2, 3] = false; 
