typeof lambda x : Int. lambda y : Int. x + y;
typeof 1 = 2 || 6 = 3 * 2 - 5 - 6 * 1;
typeof lambda x : Int -> Bool -> Unit. x;
typeof let x = 5 = 5 in x;

exception arithexc of Unit in
let div = lambda x : Int. lambda y : Int. if y = 0 then throw arithexc unit as Int else x/y in
try div 2 0 catch {arithexc _ => 42}
   {arithexc c => 52};

exception ex1 of Unit in try throw ex1 unit as Int catch {ex1 _ => 43};

typeof (lambda x : Int . 5 * x) 5;
fix (lambda f : Int -> Int. lambda n : Int. if n=0 then 1 else (n * (f (n-1)))) 0;
fix (lambda f : Int -> Int. lambda n : Int. if n=0 then 1 else (n * (f (n-1)))) 2;
fix (lambda f : Int -> Int. lambda n : Int. if n=0 then 1 else (n * (f (n-1)))) 3;
fix (lambda f : Int -> Int. lambda n : Int. if n=0 then 1 else (n * (f (n-1)))) 10;