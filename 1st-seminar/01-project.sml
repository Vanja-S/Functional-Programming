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

fun neg True = False
|   neg False = True
|   neg (Not e) = e
|   neg e = Not e;

(* exception for nonimplemented functions *)
exception NotImplemented;


(* ==================== HELPER FUN. ==================== *)

infixr 1 $;
fun f $ x = f x;

fun eq a b = a = b;

fun neq a b = a <> b;

fun remove x = List.filter (neq x);

fun memberOf (item: ''a) (xs: ''a list) = List.exists (fn x => x = item) xs;

fun expressionToString toString =
    let fun exprToStr e = 
        case e of
            Not expr => "Not(" ^ exprToStr expr ^ ")"
          | Or exprs => "Or[" ^ String.concatWith ", " (map exprToStr exprs) ^ "]"
          | And exprs => "And[" ^ String.concatWith ", " (map exprToStr exprs) ^ "]"
          | Eq exprs => "Eq[" ^ String.concatWith ", " (map exprToStr exprs) ^ "]"
          | Imp(left, right) => "Imp(" ^ exprToStr left ^ ", " ^ exprToStr right ^ ")"
          | Var v => toString v
          | True => "True"
          | False => "False"
    in
        exprToStr
    end;

(* Generic list printing function *)
fun printExprList toString [] = print "Empty list\n"
  | printExprList toString exprs = 
      let
        val strExpr = expressionToString toString
        val strs = map strExpr exprs
        val result = "[" ^ String.concatWith ", " strs ^ "]\n"
      in
        print result
      end;

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

fun allEquivalent [] = true
  | allEquivalent [x] = true
  | allEquivalent (x::xs) = List.all (fn y => y = x) xs;

fun eval (vars: ''a list) (Var(var)) = if (memberOf var vars) then true else false
  | eval (vars: ''a list) (Not(e)) = not (eval (vars) (e))
  | eval (vars: ''a list) (True) = true
  | eval (vars: ''a list) (False) = false  
  | eval (vars: ''a list) (Or([])) = false
  | eval (vars: ''a list) (And([])) = true
  | eval (vars: ''a list) (Eq([])) = true
  | eval (vars: ''a list) (Or([e])) = eval (vars) (e)
  | eval (vars: ''a list) (And([e])) = eval (vars) (e)
  | eval (vars: ''a list) (Eq([e])) = true
  | eval (vars: ''a list) (Or exprs) = List.exists (eval vars) exprs
  | eval (vars: ''a list) (And exprs) = List.all (eval vars) exprs
  | eval (vars: ''a list) (Eq exprs) = 
        let
            val evaluated = List.map (eval vars) exprs
        in
            allEquivalent evaluated
        end
  | eval (vars: ''a list) (Imp (left, right)) = not (eval vars left) orelse (eval vars right);

fun rmEmpty (Imp (left, right)) = Imp (rmEmpty left, rmEmpty right)
  | rmEmpty (Not expr) = Not (rmEmpty expr)
  | rmEmpty (Or exprs) =
      let
        val simplified = List.map rmEmpty exprs
      in
        case simplified of
          [] => False
        | [x] => x 
        | xs => Or xs
      end
  | rmEmpty (And exprs) =
      let
        val simplified = List.map rmEmpty exprs
      in
        case simplified of
          [] => True
        | [x] => x
        | xs => And xs
      end
  | rmEmpty (Eq exprs) =
      let
        val simplified = List.map rmEmpty exprs
      in
        case simplified of
          [] => True
        | [x] => True
        | xs => Eq xs
      end
  | rmEmpty (True) = True
  | rmEmpty (False) = False
  | rmEmpty (Var var) = Var (var);

fun pushNegations (e: 'a expression) = 
    let
      val simplified = rmEmpty e
      fun pushNegationHelper (Not (Not expr)) = pushNegationHelper (expr)
        | pushNegationHelper (Not (Or exprs)) = And (List.map (fn (x) => pushNegationHelper (Not (x))) exprs)
        | pushNegationHelper (Not (And exprs)) = Or (List.map (fn (x) => pushNegationHelper (Not (x))) exprs)
        | pushNegationHelper (Not (Imp (left, right))) = And ([pushNegationHelper (left), pushNegationHelper (Not (right))])
        | pushNegationHelper (Not (Eq exprs)) = And ([Or (List.map (fn (x) => pushNegationHelper (Not (x))) exprs), Or (List.map (fn (x) => pushNegationHelper x) exprs) ])
        | pushNegationHelper (Not expr) = Not (pushNegationHelper expr)
        | pushNegationHelper (Var var) = Var (var)
        | pushNegationHelper (True) = True
        | pushNegationHelper (False) = False
        | pushNegationHelper (Or exprs) = Or (List.map pushNegationHelper exprs)
        | pushNegationHelper (And exprs) = And (List.map pushNegationHelper exprs)
        | pushNegationHelper (Eq exprs) = Eq (List.map pushNegationHelper exprs)
        | pushNegationHelper (Imp (left, right)) = Imp (pushNegationHelper (left), pushNegationHelper (right))
    in
      pushNegationHelper simplified
    end;

fun rmConstants (e: ''a expression) =
  let
    val simplified = rmEmpty e
    fun rmConstantsHelper (And exprs) =
      let
        val simplified_exprs = List.map rmConstantsHelper exprs
        val non_true = List.filter (fn x => x <> True) simplified_exprs
      in
        if List.exists (fn x => x = False) simplified_exprs then False
        else case non_true of
          [] => True
        | [x] => x
        | xs => And xs
      end
    | rmConstantsHelper (Or exprs) =
      let
        val simplified_exprs = List.map rmConstantsHelper exprs
        val non_false = List.filter (fn x => x <> False) simplified_exprs
      in
        if List.exists (fn x => x = True) simplified_exprs then True
        else case non_false of
          [] => False
        | [x] => x
        | xs => Or xs
      end
    | rmConstantsHelper (Eq exprs) =
      let
        val simplified_exprs = List.map rmConstantsHelper exprs
        val non_constants = List.filter (fn x => x <> True andalso x <> False) simplified_exprs
        val has_true = List.exists (fn x => x = True) simplified_exprs
        val has_false = List.exists (fn x => x = False) simplified_exprs
      in
        case (non_constants, has_true, has_false) of
          ([], _, _) => if has_true = has_false then True else False
        | (_, true, true) => False
        | (_, true, false) => 
            (case non_constants of 
              [x] => x
            | xs => And xs)
        | (_, false, true) => 
            (case non_constants of 
              [x] => Not x
            | xs => And (List.map Not xs))
        | _ => Eq non_constants
      end
    | rmConstantsHelper (Imp (left, right)) =
      let
        val left' = rmConstantsHelper left
        val right' = rmConstantsHelper right
      in
        case (left', right') of
          (False, _) => True
          | (_, True) => True
          | (True, x) => x
          | (x, False) => Not x
          | _ => Imp (left', right')
      end
    | rmConstantsHelper (Not expr) =
      let
        val simplified = rmConstantsHelper expr
      in
        case simplified of
          True => False
        | False => True
        | _ => Not simplified
      end
    | rmConstantsHelper (Var x) = Var x
    | rmConstantsHelper True = True
    | rmConstantsHelper False = False
  in
    rmConstantsHelper simplified
  end;

fun rmVars (e: ''a expression) =
  let
    val simplified = rmEmpty e
    fun rmVarsHelper (And exprs) =
      let
        val simplified_exprs = List.map rmVarsHelper exprs
        val unique_exprs = isolate simplified_exprs
      in
        case unique_exprs of
          [] => True
        | [x] => x
        | xs => And xs
      end
    | rmVarsHelper (Or exprs) =
      let
        val simplified_exprs = List.map rmVarsHelper exprs
        val unique_exprs = isolate simplified_exprs
      in
        case unique_exprs of
          [] => False
        | [x] => x
        | xs => Or xs
      end
    | rmVarsHelper (Eq exprs) =
      let
        val simplified_exprs = List.map rmVarsHelper exprs
        val unique_exprs = isolate simplified_exprs
      in
        case unique_exprs of
          [] => True
        | [x] => True
        | xs => Eq xs
      end
    | rmVarsHelper (Imp (left, right)) =
      let
        val left' = rmVarsHelper left
        val right' = rmVarsHelper right
      in
        if left' = right' then True 
        else Imp (left', right')
      end
    | rmVarsHelper (Not expr) = Not (rmVarsHelper expr)
    | rmVarsHelper (Var x) = Var x
    | rmVarsHelper True = True
    | rmVarsHelper False = False
  in
    rmVarsHelper simplified
  end;

fun simplify (e: ''a expression) = 
  let 
    fun applyAll expr = rmVars (pushNegations (rmConstants expr))
    fun simplifyHelper expr =
      let
        val simplified = applyAll expr
      in
        if eq expr simplified then expr
        else simplifyHelper simplified
      end
  in
    simplifyHelper e
  end;

fun filterRandom (seed, lst) =
    let
        val randomStream = ref (lcg seed)
        fun getNextBool () =
            let
                val Next(x, f) = !randomStream
                val _ = randomStream := f()
            in
                int2bool x
            end
    in
        List.filter (fn x => getNextBool()) lst
    end;

fun prTestEq (seed: int) (expr1: ''a expression) (expr2: ''a expression) = 
  let
    val vars1 = getVars (expr1)
    val vars2 = getVars (expr2)
    val vars = isolate (vars1 @ vars2)
    val trueVars = filterRandom (seed, vars)
    val exprEval1 = eval trueVars expr1
    val exprEval2 = eval trueVars expr2
  in
    eq exprEval1 exprEval2
  end;

fun singleton (Or [Var x]) = SOME (x, true)
  | singleton (Or [Not (Var x)]) = SOME (x, false)
  | singleton (Var x) = SOME (x, true)
  | singleton (Not (Var x)) = SOME (x, false)
  | singleton _ = NONE;

fun setVar var value expr =
  let
    fun replace e =
      case e of
        Var x =>
          if x = var 
          then (if value then True else False)
          else e
      | Not (Var x) =>
          if x = var 
          then (if value then False else True)
          else Not (Var x)
      | Not e' =>
          let val replaced_e = replace e'
          in case replaced_e of
               True => False
             | False => True
             | _ => Not replaced_e
          end
      | And es =>
          let 
            val es' = List.map replace es
            val es'' = List.filter (fn x => x <> True) es'
          in
            if List.exists (fn x => x = False) es' 
            then False
            else 
              case es'' of
                [] => True
              | [single] => single
              | _ => And es''
          end
      | Or es =>
          let 
            val es' = List.map replace es
            val es'' = List.filter (fn x => x <> False) es'
          in
            if List.exists (fn x => x = True) es' 
            then True
            else 
              case es'' of
                [] => False
              | [single] => single
              | _ => Or es''
          end
      | Imp (e1, e2) =>
          let 
            val e1' = replace e1
            val e2' = replace e2
          in
            case (e1', e2') of
              (False, _) => True
            | (_, True) => True
            | (True, e2'') => e2''
            | (_, False) => Not e1'
            | _ => Imp (e1', e2')
          end
      | Eq es =>
          let 
            val es' = List.map replace es
            val values = List.filter (fn x => x = True orelse x = False) es'
            val others = List.filter (fn x => x <> True andalso x <> False) es'
          in
            if List.exists (fn x => x = True) values andalso 
               List.exists (fn x => x = False) values 
            then False
            else if null others 
            then True
            else Eq others
          end
      | _ => e
  in
    replace expr
  end
 
fun rmSingletons expr_out vars_out =
  let
    fun loop currentExpr currentVars =
      case singleton currentExpr of
        SOME (x, value) =>
          let
            val newExpr = setVar x value currentExpr
            val newVars = if value then x::currentVars else currentVars
          in
            loop newExpr newVars
          end
      | NONE =>
          case currentExpr of
            And es =>
              let
                fun processList [] expr vars = (expr, vars)
                  | processList (e::rest) expr vars =
                      case singleton e of
                        SOME (x, value) =>
                          let
                            val updatedExpr = setVar x value expr
                            val updatedVars = if value then x::vars else vars
                          in
                            loop updatedExpr updatedVars
                          end
                      | NONE =>
                          processList rest expr vars
              in
                processList es currentExpr currentVars
              end
          | _ => (currentExpr, currentVars)
  in
    loop expr_out vars_out
  end;

fun dpll expr vars =
  let
    val (currentExpr, currentVars) = rmSingletons expr vars
  in
    (case currentExpr of
        True => SOME currentVars
      | False => NONE
      | And [] => SOME currentVars
      | Or [] => NONE
      | And es =>
          if List.exists (fn e => e = Or []) es then NONE
          else
            let
              val varOpt =
                let
                  fun findVar e =
                    case e of
                        Var x => SOME x
                      | Not (Var x) => SOME x
                      | Or es' => findVarInList es'
                      | And es' => findVarInList es'
                      | _ => NONE
                  and findVarInList [] = NONE
                    | findVarInList (e::rest) =
                          case findVar e of
                              SOME x => SOME x
                            | NONE => findVarInList rest
                in
                  findVar currentExpr
                end
              val var =
                  case varOpt of
                      SOME x => x
                    | NONE => raise Fail "No variable found"
              val exprTrue = setVar var true currentExpr
              val resTrue = dpll exprTrue (var::currentVars)
            in
              (case resTrue of
                  SOME varsRes => SOME varsRes
                | NONE =>
                    let
                      val exprFalse = setVar var false currentExpr
                      val resFalse = dpll exprFalse currentVars
                    in
                      resFalse
                    end)
            end
      | Or es =>
          if currentExpr = Or [] then NONE
          else
            let
              val varOpt =
                let
                  fun findVarInList [] = NONE
                    | findVarInList (e::rest) =
                        case e of
                            Var x => SOME x
                          | Not (Var x) => SOME x
                          | _ => findVarInList rest
                in
                  findVarInList es
                end
              val var =
                  (case varOpt of
                      SOME x => x
                    | NONE => raise Fail "No variable found in Or expression")
              val exprTrue = setVar var true currentExpr
              val resTrue = dpll exprTrue (var::currentVars)
            in
              (case resTrue of
                  SOME varsRes => SOME varsRes
                | NONE =>
                    let
                      val exprFalse = setVar var false currentExpr
                      val resFalse = dpll exprFalse currentVars
                    in
                      resFalse
                    end)
            end
      | _ => raise Fail "Invalid expression in DPLL")
  end;

fun satSolver (expr: ''a expression) =
  if not (isCNF expr) then 
    raise InvalidCNF
  else
    case dpll expr [] of
        SOME vars => SOME (List.rev vars)
      | NONE => NONE;



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


fun problemReduction _ _ _ = raise NotImplemented

fun solutionRepresentation _ = raise NotImplemented;