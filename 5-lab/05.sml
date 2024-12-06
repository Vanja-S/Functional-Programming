signature RATIONAL =
sig
    (* Definirajte podatkovni tip rational, ki podpira preverjanje enakosti. *)
    eqtype rational

    (* Definirajte izjemo, ki se kliče pri delu z neveljavnimi ulomki - deljenju z nič. *)
    exception BadRational

    (* Vrne racionalno število, ki je rezultat deljenja dveh podanih celih števil. *)
    val makeRational: int * int -> rational

    (* Vrne nasprotno vrednost podanega števila. *)
    val neg: rational -> rational

    (* Vrne obratno vrednost podanega števila. *)
    val inv: rational -> rational

    (* Funkcije za seštevanje in množenje. Rezultat vsake operacije naj sledi postavljenim pravilom. *)
    val add: rational * rational -> rational
    val mul: rational * rational -> rational

    (* Vrne niz, ki ustreza podanemu številu.
        Če je število celo, naj vrne niz oblike "x" oz. "~x".
        Če je število ulomek, naj vrne niz oblike "x/y" oz. "~x/y". *)
    val toString: rational -> string
end

structure Rational :> RATIONAL = 
struct
  datatype rational = 
      Whole of int
    | Frac of int * int;
  exception BadRational

  fun gcd (a: int, b: int): int =
    let val (x, y) = (abs a, abs b)
    in
        if y = 0 then x
        else gcd (y, x mod y)
    end

  fun makeRational (a: int, b: int) =
    if b = 0 then 
      raise BadRational
    else if b < 0 then
      Frac (~a, ~b)
    else if b = 1 then
      Whole a
    else 
      let 
        val (na, nb) = if b < 0 then (~a, ~b) else (a, b)
        
        val divisor = gcd (na, nb)
        val reducedNum = na div divisor
        val reducedDen = nb div divisor
      in
        if reducedDen = 1 then
          Whole reducedNum
        else
          Frac (reducedNum, reducedDen)
      end

  fun neg (Whole a) = Whole (~a)
    | neg (Frac (a,b)) = Frac (~a, ~b)

  fun inv (Whole a) = Frac (1, a)
    | inv (Frac (a,b)) = Frac (b,a)

  fun add (Whole a, Whole b) = Whole (a + b)
    | add (Whole a, Frac (num, den)) = Frac (a * den + num, den)
    | add (Frac (num, den), Whole b) = Frac (b * den + num, den)
    | add (Frac (a1, b1), Frac (a2, b2)) = 
        let 
          val newNum = a1 * b2 + a2 * b1
          val newDen = b1 * b2
          val divisor = gcd (newNum, newDen)
        in
          if newDen div divisor = 1 then 
            Whole (newNum div divisor)
          else 
            Frac (newNum div divisor, newDen div divisor)
        end
  
  fun mul (Whole a, Whole b) = Whole (a * b)
    | mul (Whole a, Frac (num, den)) = 
        let 
          val newNum = a * num
          val divisor = gcd (newNum, den)
        in
          if den div divisor = 1 then
            Whole (newNum div divisor)
          else
            Frac (newNum div divisor, den div divisor)
        end
    | mul (Frac (num, den), Whole b) = 
        let 
          val newNum = b * num
          val divisor = gcd (newNum, den)
        in
          if den div divisor = 1 then
            Whole (newNum div divisor)
          else
            Frac (newNum div divisor, den div divisor)
        end
    | mul (Frac (a1, b1), Frac (a2, b2)) = 
        let 
          val newNum = a1 * a2
          val newDen = b1 * b2
          val divisor = gcd (newNum, newDen)
        in
          if newDen div divisor = 1 then
            Whole (newNum div divisor)
          else
            Frac (newNum div divisor, newDen div divisor)
        end

  fun toString (Whole n) = 
        if n = 0 then "0"
        else if n > 0 then Int.toString n
        else "~" ^ Int.toString(~n)
    | toString (Frac (num, den)) =
        let 
          val numStr = Int.toString(abs num)
          val denStr = Int.toString(den)
        in
          if num = 0 then "0"
          else if num > 0 then numStr ^ "/" ^ denStr
          else "~" ^ numStr ^ "/" ^ denStr
        end
end

signature EQ =
sig
    type t
    val eq : t -> t -> bool
end

signature SET =
sig
    (* podatkovni tip za elemente množice *)
    type item

    (* podatkovni tip množico *)
    type set

    (* prazna množica *)
    val empty : set

    (* vrne množico s samo podanim elementom *)
    val singleton : item -> set

    (* unija množic *)
    val union : set -> set -> set

    (* razlika množic (prva - druga) *)
    val difference : set -> set -> set

    (* a je prva množica podmnožica druge *)
    val subset : set -> set -> bool
end

funsig SETFN (Eq : EQ) = SET

functor SetFn (Eq : EQ) : SET =
struct
    type item = Eq.t
    type set = item list

    val empty = []

    fun singleton x = [x]

    fun contains (x, []) = false
      | contains (x, y::ys) = Eq.eq x y orelse contains (x, ys)

    fun union [] s2 = s2
      | union (x::xs) s2 = 
        if contains (x, s2) 
        then union xs s2 
        else x :: union xs s2

    fun difference [] _ = []
      | difference (x::xs) s2 = 
        if contains (x, s2)
        then difference xs s2
        else x :: difference xs s2

    fun subset [] _ = true
      | subset (x::xs) s2 = 
        contains (x, s2) andalso subset xs s2
end