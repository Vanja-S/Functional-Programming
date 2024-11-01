datatype number = Zero | Succ of number | Pred of number;

fun count number =
    case number of
        Zero => 0
      | Succ n => 1 + count n
      | Pred n => ~1 + count n;

fun build n =
    if n = 0 then Zero
    else if n > 0 then Succ (build (n - 1))
    else Pred (build (n + 1));

fun simp number =
    build (count number);

(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (a : number) = 
    case a of 
        Zero => Zero
        | Succ n => Pred (neg (n))
        | Pred n => Succ (neg (n));

(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! *)
fun add (Zero, Zero) = Zero
  | add (a: number, b: number) = 
        let 
            val sim_a = simp(a)
            val sim_b = simp(b)
        in
            case sim_b of
                Zero => sim_a
              | Succ n => simp (Succ (add (a, n)))
              | Pred n => simp (Pred (add (a,n)))
        end;

datatype order = LESS | EQUAL | GREATER

(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)
fun comp (a : number, b : number) : order =
    let
        val a_simp = simp a
        val b_simp = simp b
    in
        case (a_simp, b_simp) of
            (Zero, Zero) => EQUAL
          | (Succ _, Pred _) => GREATER
          | (Pred _, Succ _) => LESS
          | (Succ a', Succ b') => comp (a', b')
          | (Pred a', Pred b') => comp (a', b')
          | (Succ _, Zero) => GREATER
          | (Pred _, Zero) => LESS
          | (Zero, Succ _) => LESS
          | (Zero, Pred _) => GREATER
    end;


datatype tree = Node of int * tree * tree | Leaf of int;

(* Vrne true, če drevo vsebuje element x. *)
fun contains (Leaf value, x : int) = (value = x)
  | contains (Node (value, left, right), x) = 
        value = x orelse contains (left, x) orelse contains (right, x);

(* Vrne število listov v drevesu. *)
fun countLeaves (tree : tree) = 
    case tree of
        Leaf _ => 1
      | Node (_, left, right) => countLeaves(left) + countLeaves(right);

(* Vrne število število vej v drevesu. *)
fun countBranches (Leaf _) = 0
  | countBranches (Node (_, Leaf _, Leaf _)) = 2
  | countBranches (Node (_, left, right)) = 2 + countBranches(left) + countBranches(right);

(* Vrne višino drevesa. Višina lista je 1. *)
fun height (tree : tree) =
    case tree of
        Leaf _ => 1
      | Node (_, left, right) => 1 + Int.max (height left, height right);

(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
fun toList (Leaf value) = [value]
  | toList (Node (value, left, right)) = toList (left) @ [value] @ toList (right);

(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
fun isBalanced (Leaf _) = true
  | isBalanced (Node (_, left, right)) =
        let
            val diff = abs (height (left) - height (right)) 
        in
            (diff < 1) orelse (diff = 1)
        end;

(* Helper function to check if a value is within a given range *)
fun isWithinRange (v : int, min : int, max : int) : bool =
    v > min andalso v < max;

(* Recursively check if the tree is a BST within the specified value range *)
fun isBSTHelper (Leaf v, min, max) = true  (* Leaves are always BSTs *)
  | isBSTHelper (Node (v, left, right), min, max) =
      isWithinRange (v, min, max) andalso
      isBSTHelper (left, min, v) andalso 
      isBSTHelper (right, v, max);

(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)
fun isBST (tree : tree) = isBSTHelper (t, ~Inf, Inf);