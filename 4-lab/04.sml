fun reduce (f:('a -> 'b -> 'a)) (z:'a) [] = z
  | reduce (f:('a -> 'b -> 'a)) (z:'a) (x::xs: 'b list) = reduce f (f z x) xs;

fun squares [] = []
  | squares xs: int list = List.map (fn x => x * x ) xs;

fun onlyEven [] = []
  | onlyEven xs: int list = List.filter (fn x => x mod 2 = 0) xs;

fun bestString (f: (string * string -> bool)) ([]) = ""
  | bestString (f: (string * string -> bool)) (xs: string list) = 
        List.foldl (fn (x, acc) => if f(x, acc) then x else acc) (hd xs) (tl xs)

fun largestString (xs: string list) = bestString (fn (s1, s2) => s1 > s2) xs;

fun longestString (xs: string list) = bestString (fn (s1, s2) => if String.size s1 >= String.size s2 then true else false) xs;

fun quicksort (f: ('a * 'a -> order)) [] = []
  | quicksort (f: ('a * 'a -> order)) [x] = [x]
  | quicksort (f: ('a * 'a -> order)) (pivot::xs: 'a list) =
    let
        val (less, greater) = List.partition (fn x => f (x, pivot) = LESS) xs
    in
        (quicksort f less) @ [pivot] @ (quicksort f greater)
    end;

fun dot [] [] = 0
  | dot (xs: int list) (ys: int list) = List.foldr (fn (x, acc) => acc + x ) 0 (ListPair.map (fn (x, y) => x * y) (xs, ys));

fun transpose [] = []
  | transpose ([]::_) = []
  | transpose rows =
    let
        val firstCol = List.map hd rows
        val restRows = List.map tl rows
    in
        firstCol :: transpose restRows
    end;

fun multiply m1 m2 = 
    let 
        val m2T = transpose m2
    in
        List.map (fn row => List.map (fn col => dot row col) m2T) m1
    end;

fun group [] = []
  | group (x::xs) =
        let
            val rec countHelper = 
                fn (x, [], acc) => [(x, acc)]
                 | (x, y::ys, acc) => 
                    if x = y 
                    then countHelper(x, ys, acc + 1)
                    else (x, acc) :: countHelper(y, ys, 1)
        in
            countHelper(x, xs, 1)
        end;

fun equivalenceClasses f xs =
    List.foldl 
        (fn (x, acc) =>
              case List.partition (fn cls => f x (List.hd cls)) acc of
                  ([], rest) => [x] :: rest
                | (matching, rest) => (x :: List.hd matching) :: rest)
          []
          xs;