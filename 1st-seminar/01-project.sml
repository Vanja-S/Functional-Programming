(* settings for long expressions *)
val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;
(* disable polyEq warnings *)
val _ = Control.polyEqWarn := false;



(* datatype for logical formulas *)
datatype 'a expression = 
    Not of 'a expression
|   Or of 'a expression list
|   And of 'a expression list
|   Eq of 'a expression list
|   Imp of 'a expression * 'a expression
|   Var of 'a
|   True | False;


(* linear congurence random number generator for function `prTestEq` *)
datatype 'a stream = Next of 'a * (unit -> 'a stream);

fun lcg seed =
    let fun lcg seed =
        Next (seed, fn () =>
            lcg (LargeInt.mod (1103515245 * seed + 12345, 0x7FFFFFFF)))
    in lcg (LargeInt.fromInt seed)
    end;

fun int2bool i = LargeInt.mod (i, 2) = 1;


(* conjutive normal form tester for function `satSolver` *)
fun isCNF (And es) =
    List.all
        (fn Or es => List.all (fn (Var _ | Not (Var _)) => true | _ => false) es
        |   (Var _ | Not (Var _)) => true
        |   _ => false) es
|   isCNF (Or es) = List.all (fn (Var _ | Not (Var _)) => true | _ => false) es
|   isCNF (True | False | Var _ | Not (Var _)) = true
|   isCNF _ = false;
(* exception for function `satSolver` *)
exception InvalidCNF;


(* ==================== SOME HELPER FUN. ==================== *)

(* operator for low priority right associative applications *)
infixr 1 $;
fun f $ x = f x;

(* curried equlity test *)
fun eq a b = a = b;

(* curried inequlity test *)
fun neq a b = a <> b;

(* removes all occurrences of `x` from a list *)
fun remove x = List.filter (neq x);

(* exception for nonimplemented functions *)
exception NotImplemented;


(* ==================== HELPER FUN. ==================== *)

infixr 1 $;
fun f $ x = f x;

fun eq a b = a = b;

fun neq a b = a <> b;

fun remove x = List.filter (neq x);


(* ==================== WARMUP ==================== *)

fun isolate lst =
  let
      fun isolateHelper ([], acc) = List.rev acc  
          | isolateHelper (x::xs, acc) =
              if List.exists (fn y => y = x) acc then
                  isolateHelper (xs, acc)
              else
                  isolateHelper (xs, x::acc)
  in
      isolateHelper (lst, [])
  end;


(* ==================== PART 1 ==================== *)



fun getVars (e: ''a expression) =
  let 
    fun getVarsHelper (Var(var)) = [var]
      | getVarsHelper (Not(e)) = getVarsHelper (e)
      | getVarsHelper (Or([])) = []
      | getVarsHelper (And([])) = []
      | getVarsHelper (Eq([])) = []
      | getVarsHelper (Or(head::tail)) = getVarsHelper (head) @ getVarsHelper (Or(tail))
      | getVarsHelper (And(head::tail)) = getVarsHelper (head) @ getVarsHelper (And(tail))
      | getVarsHelper (Eq(head::tail)) = getVarsHelper (head) @ getVarsHelper (And(tail))
      | getVarsHelper (Imp(e_1, e_2)) = getVarsHelper (e_1) @ getVarsHelper (e_2)
      | getVarsHelper (True) = []
      | getVarsHelper (False) = []
  in
    isolate(getVarsHelper (e))
  end;

fun eval _ _ = raise NotImplemented;

fun rmEmpty _ = raise NotImplemented;

fun pushNegations _ = raise NotImplemented;

fun rmConstants _ = raise NotImplemented;

fun rmVars _ = raise NotImplemented;

fun simplify _ = raise NotImplemented;

fun prTestEq _ _ _ = raise NotImplemented;

fun satSolver _ = raise NotImplemented;

(*  Za namene terstiranja drugega dela seminarske naloge odkomentiraj
    spodnjo kodo v primeru, da funkcije satSolver nisi implementiral.
    Pred oddajo odstrani nasledji dve vrstici kode!
    Deluje samo za izraze oblike `And [Or [...], Or [...], ....]`*)

(* use "external_sat_solver.sml";
val satSolver = external_sat_solver;
 *)


(* ==================== PART 2 ==================== *)

type timetable = {day : string, time: int, course: string} list;
type student = {studentID : int, curriculum : string list};


fun problemReduction _ _ _ = raise NotImplemented;

fun solutionRepresentation _ = raise NotImplemented;