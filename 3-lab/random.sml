val u = 1
fun f v =
    let
    val u = v + 1
    in
    fn w => u + v + w
    end;
val u = 3;
val g = f 4;
val v = 5;
val w = g 6;