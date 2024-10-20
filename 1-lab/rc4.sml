fun ksa (key : int list) = 
    let
        val S = List.tabulate (256, fn i: int => i)
        fun permuteS (S : int list, j : int, i : int) =
            if i >= 256 then
                S
            else
                let
                    val j_1 = (j + List.nth (S, i) + List.nth (key, i mod length key)) mod 256  
                    val temp = List.nth (S, i)
                    val S1 = List.update (S, i, List.nth (S, j_1))
                    val S2 = List.update (S1, j_1, temp) 
                in
                    permuteS (S2, j_1, i + 1)
                end
    in
        permuteS(S, 0, 0)
    end;

fun rc4 (key : int list, plaintext : int list) : int list =
    let
        val S = ksa key  
        fun rc4Helper (i : int, j : int, [] : int list) = []  
          | rc4Helper (i, j, char::rest) =
                let
                    val i' = (i + 1) mod 256
                    val j' = (j + List.nth(S, i')) mod 256  
                    
                    val temp = List.nth(S, i')
                    val updatedS1 = update(S, i', List.nth(S, j'))
                    val updatedS2 = update(updatedS1, j', temp)

                    val t = (List.nth(updatedS2, i') + List.nth(updatedS2, j')) mod 256  
                    val K = List.nth(updatedS2, t)  
                    val outputChar = K xor char 
                in
                    outputChar :: rc4Helper (i', j', rest)
                end
    in
        rc4Helper (0, 0, plaintext)
    end;