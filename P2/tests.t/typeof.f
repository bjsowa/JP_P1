typeof 5 + 6;
typeof 5 + if false then 0 else 5;
typeof 5 + lambda x : Int. lambda y : Int. x + y;
typeof lambda x : Int. lambda y : Int. x + y;
typeof 1 = 2 || 6 = 3 * 2 - 5 - 6 * 1;
typeof lambda x : Int -> Bool -> Unit. x;
typeof let x = 5 = 5 in x;

typeof
exception arithexc of Unit in
let div = lambda x : Int. lambda y : Int. if y = 0 then throw arithexc unit as Int else x/y in
try div 2 0 catch {arithexc _ => 42};