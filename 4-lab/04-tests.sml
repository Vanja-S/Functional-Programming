val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;

val _ = print "~~~~~~~~ reduce ~~~~~~~~\n";
val test_type: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = reduce;
val test = reduce (fn acc => fn x => acc + x) 0 [1,2,3,4] = 10;

val _ = print "~~~~~~~~ squares ~~~~~~~~\n";
val test_type: int list -> int list = squares;
val test = squares ([0,1,2,3]) = [0,1,4,9];

val _ = print "~~~~~~~~ onlyEven ~~~~~~~~\n";
val test_type: int list -> int list = onlyEven;
val test = onlyEven ([0,1,2,3]) = [0,2];

val _ = print "~~~~~~~~ bestString ~~~~~~~~\n";
val test_type: (string * string -> bool) -> string list -> string = bestString;

val _ = print "~~~~~~~~ largestString ~~~~~~~~\n";
val test_type: string list -> string = largestString;
val test = largestString (["apple", "zebra", "banana"]) = "zebra";

val _ = print "~~~~~~~~ longestString ~~~~~~~~\n";
val test_type: string list -> string = longestString;
val test = longestString (["apple", "zebra", "banana"]) = "banana";

val _ = print "~~~~~~~~ quicksort ~~~~~~~~\n";
val test_type: ('a * 'a -> order) -> 'a list -> 'a list = quicksort;
fun intOrder (a, b) = 
    if a < b then
        LESS
    else if a > b then
        GREATER
    else 
        EQUAL;
val test = quicksort intOrder [1,4,10,6,3,2,7,5,9,8] = [1,2,3,4,5,6,7,8,9,10];

val _ = print "~~~~~~~~ dot ~~~~~~~~\n";
val test_type: int list -> int list -> int = dot;
val test = dot [1,2,3] [1,2,3] = 14;

val _ = print "~~~~~~~~ transpose ~~~~~~~~\n";
val test_type: 'a list list -> 'a list list = transpose;
val test = transpose [[1,2,3],[4,5,6],[7,8,9]] = [[1,4,7],[2,5,8],[3,6,9]];
val test = transpose ([]: int list list) = [];
val test1 = transpose ([] : int list list) = ([] : int list list);
val test2 = transpose [[]] = [];
val test3 = transpose [[1], [2], [3]] = [[1, 2, 3]];
val test4 = transpose [[1, 2, 3]] = [[1], [2], [3]];
val test5 = transpose [[1, 2, 3],[4, 5, 6]] = [[1, 4],[2, 5],[3, 6]];
 
val _ = print "~~~~~~~~ multiply ~~~~~~~~\n";
val test_type: int list list -> int list list -> int list list = multiply;
val test1 = multiply [] [] = [];
val test2 = multiply [[1]] [[2]] = [[2]];
val test3 = multiply [[1,2],
                    [3,4]] 
                   [[5,6],
                    [7,8]] = [[19,22],
                             [43,50]];
val test4 = multiply [[1,2,3],
                    [4,5,6]] 
                   [[7,8],
                    [9,10],
                    [11,12]] = [[58,64],
                               [139,154]];
val test5 = multiply [[1,0],
                    [0,1]] 
                   [[5,6],
                    [7,8]] = [[5,6],
                             [7,8]];
 
val _ = print "~~~~~~~~ equivalenceClasses ~~~~~~~~\n";
val test_type: ('a -> 'a -> bool) -> 'a list -> 'a list list = equivalenceClasses;

