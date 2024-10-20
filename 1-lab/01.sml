(*  Vrne fakulteto števila n, n >= 0. *)
fun factorial (n : int) = 
    if n = 0 then
        1
    else if n > 0 then
        n * factorial(n - 1)
    else
        raise Fail "Factorial is not defined for negative numbers";

 (* Vrne n-to potenco števila x, n >= 0.*)
fun power (x : int, n : int) = 
    if n = 0 then
        1
    else
        x * power (x, n - 1);

(*  Vrne največjega skupnega delitelja pozitivnih števil a in b, a >= b. *)
fun gcd (a : int, b : int) = 
    if b > 0 then let
            val x = b
            val y = a mod b
        in
            gcd(x, y)
        end
    else a;

(*  Vrne dolžino seznama. *)
fun len (xs : int list) = 
    length xs;

(*  Vrne SOME zadnji element seznama. Če je seznam prazen vrne NONE. *)
fun last (xs : int list) = 
    if null xs then
        NONE
    else if length xs = 1 then
        SOME (hd xs)
    else last(tl xs);

(* To 'pattern matching' sintakso sem našel v dokumentaciji, zdi se mi bolj clean kot ogromno if stavkov *)
(*  Vrne SOME n-ti element seznama. Prvi element ima indeks 0. Če indeks ni veljaven, vrne NONE. *)
fun nth ([] : int list, _ : int) = NONE
  | nth (x::_ : int list, 0) = SOME x
  | nth (xs : int list, n) = nth (tl xs, n-1);

(*  Vrne nov seznam, ki je tak kot vhodni, le da je na n-to mesto vrinjen element x. Prvo mesto v seznamu ima indeks 0. Indeks n je veljaven (0 <= n <= length xs). *)
fun insert (xs : int list, 0, x : int) = x::xs
  | insert (xs : int list, n : int, x : int) = hd xs::insert (tl xs, n - 1, x);

(*  Vrne nov seznam, ki je tak kot vhodni, le da so vse pojavitve elementa x odstranjene. *)
fun delete ([] : int list, x : int) = []
  | delete (xs : int list, x : int) = 
        if hd xs = x then
            delete (tl xs, x)
        else 
            hd xs::delete(tl xs, x);

(*  Vrne obrnjen seznam. V pomoč si lahko spišete še funkcijo append, ki doda na konec seznama. *)
fun reverse ([] : int list) = []
  | reverse ([x] : int list) = [x]
  | reverse (xs : int list) = reverse(tl xs) @ [hd xs];

(*  Vrne true, če je podani seznam palindrom. Tudi prazen seznam je palindrom. *)
fun removeLast ([] : int list) = [] 
  | removeLast ([x] : int list) = []
  | removeLast (xs : int list) = (hd xs)::removeLast(tl xs); 

fun palindrome ([] : int list) = true
  | palindrome ([x] : int list) = true
  | palindrome (xs : int list) = 
        let
            val first = hd xs
            val last = hd (reverse xs)
        in
            first = last andalso palindrome(removeLast(tl xs))
        end;