datatype natural = Succ of natural | One;
exception NotNaturalNumber;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

fun zip ([], []) = []
  | zip ([], y: 'b list) = []
  | zip (x: 'a list, []) = []
  | zip (x: 'a list, y: 'b list) = [(hd x, hd y)] @ zip (tl x, tl y);

fun unzip ([]) = ([],[])
  | unzip ((a,b)::tail : ('a * 'b) list) = 
        let
            val (a_list, b_list) = unzip (tail)
        in
          (a::a_list, b::b_list)
        end;

fun countSucc (a: natural) =
    case a of 
        One => 0
      | Succ n => 1 + countSucc (n);

fun subtractHelper (a: natural, b: natural, acc: natural) = 
    case (a, b) of
        (One, _) => raise NotNaturalNumber
      | (Succ(a'), One) => a'
      | (Succ(a'), Succ(b')) => subtractHelper (a', b', a');

fun subtract (One, One) = raise NotNaturalNumber
  | subtract (a: natural, b: natural) = subtractHelper (a, b, a);
  
fun any (_, []) = false
  | any (f: 'a -> bool, head::tail : 'a list) = 
      if f (head) then 
        true
      else 
        any (f, tail);

fun map (_, []) = []
  | map (f: 'a -> 'b, head::tail: 'a list) = [f (head)] @ map (f, tail); 

fun filter (_, []) = []
  | filter (f: 'a -> bool, head::tail: 'a list) = 
      let 
        val condition = f (head)
      in
        if condition then
          [ head ] @ filter (f, tail)
        else 
          filter (f, tail)
      end;

fun fold (f: 'a * 'b -> 'a, z: 'a, []) = z
  | fold (f, z, head::tail) = fold (f, f(z, head), tail);

fun rotate (br(br(l_left,l_value,l_right), value, right), R) = br(l_left,l_value,br(l_right,value,right))
  | rotate (tree, R) = tree
  | rotate (br(l_left,l_value,br(l_right,value,right)), L) = br(br(l_left,l_value,l_right), value, right)
  | rotate (tree, L) = tree;

fun height lf = 0
  | height (br (left, _, right)) = 1 + Int.max (height left, height right)

fun rebalance(drevo) = 
  let
    val (h_l, h_r) = (case drevo of 
                        lf => (0, 0) |
                        br (l, x, r) => (height(l), height(r)))
    val diff = h_l - h_r
  in
    (case drevo of 
    lf => lf |
    br(l, x, r) => 
        if diff > 1
        then
            case l of
            lf => drevo |
            br(ll, lx, lr) =>
                if height ll > height lr
                then rotate(drevo, R) 
                else if height ll < height lr
                then rotate(br(rotate(l, L), x, r), R)
                else drevo 
        else if diff < ~1
        then
            case r of 
            lf => drevo |
            br(rl, rx, rr) =>
                if height rr > height rl
                then rotate(drevo, L)
                else if height rr < height rl
                then rotate(br(l, x, rotate(r, R)), L)
                else drevo
        else drevo)
end;

fun avl (c: ('a * 'a -> order), drevo: 'a bstree, e: 'a) =
  let
    fun exists (lf, e) = false
      | exists (br (l, x, r), e) = 
          case c (x, e) of
            EQUAL => true
          | LESS => exists (l, e)
          | GREATER => exists (r, e)
    fun insert (lf, e) = br (lf, e, lf)
      | insert (br (l, x, r), e) = 
          case c (e, x) of
            LESS => rebalance (br (insert (l, e), x, r))
          | GREATER => rebalance (br (l, x, insert (r, e)))
          | EQUAL => drevo
  in
    if not (exists (drevo, e))
    then insert (drevo, e)
    else drevo
  end; 