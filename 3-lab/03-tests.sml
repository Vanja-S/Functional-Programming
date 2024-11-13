(* izpis daljših izrazov v interpreterju *)
val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;


val _ = print "~~~~~~~~ zip ~~~~~~~~\n";
val test_type: 'a list * 'b list -> ('a * 'b) list = zip;
val test = zip ([1,2,3], [1,2,3]) = [(1,1), (2,2), (3,3)];
val test = zip ([1,2,3], [1,2]) = [(1,1), (2,2)];
val test = zip ([1,2,3], [1,2,3,4]) = [(1,1), (2,2), (3,3)];

val _ = print "~~~~~~~~ unzip ~~~~~~~~\n";
val test_type: ('a * 'b) list -> 'a list * 'b list = unzip;
val test = unzip ([(1,1), (2,2), (3,3)]) = ([1,2,3], [1,2,3]);

val _ = print "~~~~~~~~ subtract ~~~~~~~~\n";
val test_type: natural * natural -> natural = subtract;
val test = (subtract (One, One) handle NotNaturalNumber => One) = One;
val test = (subtract (One, Succ One) handle NotNaturalNumber => One) = One;
val test = subtract (Succ One, One) = One;
val test = subtract (Succ (Succ (Succ One)), Succ One) = Succ One;
val test = subtract (Succ (Succ (Succ One)), One) = Succ (Succ One);

val _ = print "~~~~~~~~ any ~~~~~~~~\n";
val test_type: ('a -> bool) * 'a list -> bool = any;
val test = any (fn (x: int) => x > 10, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) = false;
val test = any (fn (x: int) => x > 10, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100]) = true;

val _ = print "~~~~~~~~ map ~~~~~~~~\n";
val test_type: ('a -> 'b) * 'a list -> 'b list = map;
val test = map (fn (x: int) => if x > 5 then true else false, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) = [false, false, false, false, false , true, true, true, true, true];
val test = map (fn (x: int) => 0, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

val _ = print "~~~~~~~~ filter ~~~~~~~~\n";
val test_type: ('a -> bool) * 'a list -> 'a list = filter;
val test = filter (fn (x: int) => if x > 5 then true else false, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) = [6, 7, 8, 9, 10];

val _ = print "~~~~~~~~ fold ~~~~~~~~\n";
val test_type: ('a * 'b -> 'a) * 'a * 'b list -> 'a = fold;
val test = fold (fn (acc, x) => acc + x, 0, [1, 1, 1, 1]) = 4;

val _ = print "~~~~~~~~ rotate ~~~~~~~~\n";
val test_type: 'a bstree * direction -> 'a bstree = rotate;
val test = rotate (br(br(lf,2,lf), 1, br(lf, 3, lf)), R) = br(lf, 2, br(lf, 1, br(lf, 3, lf)));
val tr = br(br(br(lf,9,lf),17,br(lf,23,lf)), 50, br(lf,76,lf));
val test = rotate (tr, R) = br(br(lf,9,lf),17,br(br(lf,23,lf),50,br(lf,76,lf)));
val tr_1 = br(br(lf,9,lf),17,br(br(lf,23,lf),50,br(lf,76,lf)));
val test = rotate (tr_1, L) = br(br(br(lf,9,lf),17,br(lf,23,lf)), 50, br(lf,76,lf));

val _ = print "~~~~~~~~ rebalance ~~~~~~~~\n";
val test_type: 'a bstree -> 'a bstree = rebalance;

val _ = print "~~~~~~~~ avl ~~~~~~~~\n";
val test_type: ('a * 'a -> order) * 'a bstree * 'a -> 'a bstree = avl;
