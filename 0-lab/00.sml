fun next(n: int) = n + 1;
    
fun add(a: int, b: int) = a + b;
        
fun majority (a : bool, b : bool, c : bool) = (a orelse b) andalso (b orelse c);
        
fun median (a : real, b : real, c : real) = a + b + c - Real.max(Real.max(a,b), c) - Real.min(Real.min(a,b),c);
                
fun triangle (a : int, b : int, c : int) = 
    (((a + b) > c) andalso ((b + c) > a) andalso ((a + c) > b) andalso (a > 0) andalso (b > 0) andalso (c > 0));