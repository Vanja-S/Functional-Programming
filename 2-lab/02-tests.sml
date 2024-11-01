val _ = print "~~~~~~~~ simp ~~~~~~~~\n";
val test_type: number -> number = simp;
val test = simp (Pred (Succ (Succ (Pred (Pred (Succ (Pred Zero)))))));

val _ = print "~~~~~~~~ neg ~~~~~~~~\n";
val test_type: number -> number = neg;
val test = neg (Zero) = Zero;
val test = neg (Succ Zero) = Pred Zero;
val test = neg (Pred Zero) = Succ Zero;
val test = neg (Pred (Succ (Succ (Pred (Pred (Succ (Pred Zero))))))) = Succ (Pred (Pred (Succ (Succ (Pred (Succ Zero))))));
val test = neg (Succ (Succ Zero)) = Pred (Pred Zero);
val test = neg (Pred (Pred (Succ Zero))) = Succ (Succ (Pred Zero));

val _ = print "~~~~~~~~ add ~~~~~~~~\n";
val test_type: number * number -> number = add;
val test = add (Zero, Zero) = Zero;
val test = add (Succ Zero, Zero) = Succ Zero;
val test = add (Succ Zero, Succ Zero) = Succ (Succ Zero);
val test = add (Succ Zero, Pred Zero) = Zero;
val test = add (Pred (Succ (Succ (Pred (Pred (Succ (Pred Zero)))))), Succ Zero) = Zero;

val _ = print "~~~~~~~~ comp ~~~~~~~~\n";
val test_type: number * number -> order = comp;
val test = comp (Zero, Zero) = EQUAL;
val test = comp (Pred (Succ (Succ (Pred (Pred (Succ (Pred Zero)))))), Succ Zero) = LESS;


val _ = print "~~~~~~~~ contains ~~~~~~~~\n";
val test_type: tree * int -> bool = contains;
val test = contains (Leaf 1, 1) = true;
val test = contains (Node (1, Leaf 2, Leaf 3), 1) = true;
val test = contains (Node (1, Node (2, Leaf 4, Leaf 6), Node (3, Leaf 5, Leaf 7)), 7) = true;
val test = contains (Node (1, Node (2, Leaf 4, Leaf 6), Node (3, Leaf 5, Leaf 7)), ~1) = false;

val _ = print "~~~~~~~~ countLeaves ~~~~~~~~\n";
val test_type: tree -> int = countLeaves;
val test = countLeaves (Leaf 1) = 1;
val test = countLeaves (Node (1, Leaf 2, Leaf 3)) = 2;
val test = countLeaves (Node (1, Node (2, Leaf 4, Leaf 6), Node (3, Leaf 5, Leaf 7))) = 4;

val _ = print "~~~~~~~~ countBranches ~~~~~~~~\n";
val test_type: tree -> int = countBranches;
val test = countBranches (Leaf 1) = 0;
val test = countBranches (Node (1, Leaf 2, Leaf 3)) = 2;
val test = countBranches (Node (1, Node (2, Leaf 4, Leaf 6), Node (3, Leaf 5, Leaf 7))) = 6;

val _ = print "~~~~~~~~ countBranches ~~~~~~~~\n";
val test_type: tree -> int = countBranches;
val test = countBranches (Leaf 1) = 0;
val test = countBranches (Node (1, Leaf 2, Leaf 3)) = 2;
val test = countBranches (Node (1, Node (2, Leaf 4, Leaf 6), Node (3, Leaf 5, Leaf 7))) = 6;

val _ = print "~~~~~~~~ height ~~~~~~~~\n";
val test_type: tree -> int = height;
val test = height (Leaf 1) = 1;
val test = height (Node (1, Leaf 2, Leaf 3)) = 2;
val test = height (Node (1, Leaf 1, Node (3, Leaf 5, Leaf 7))) = 3;

val _ = print "~~~~~~~~ toList ~~~~~~~~\n";
val test_type: tree -> int list = toList;
val test = toList (Leaf 1) = [1];
val test = toList (Node (1, Leaf 2, Leaf 3)) = [2, 1, 3];
val test = toList (Node (1, Leaf 1, Node (3, Leaf 5, Node (7, Leaf 0, Leaf 0)))) = [1,1,5,3,0,7,0];

val _ = print "~~~~~~~~ isBalanced ~~~~~~~~\n";
val test_type: tree -> bool = isBalanced;
val test = isBalanced (Leaf 1) = true;
val test = isBalanced (Node (1, Leaf 2, Leaf 3)) = true;
val test = isBalanced (Node (1, Leaf 1, Node (3, Leaf 5, Node (7, Leaf 0, Leaf 0)))) = false;

