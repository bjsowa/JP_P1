2+2+5+2+3+5+2+3+4+2;
2+5 = 3+3;
if 5 + 5 = 10 then 5 else 10;
2 * 5 - 6 - 2 * 8 / 2;
2 = 5 || 10 = 2 * 5;
2 = 5 && 10 = 2 * 5;

(lambda x : Int. x + 1) 2;
(lambda x : Int. lambda y: Int. x + y) 2 3;
(lambda x : Int. (lambda x : Int. lambda y: Int. x + y) (x+1)) 4 5;
(lambda x : Int. lambda x : Int. x) 2 3;

let x = 42 in
let y = x in
x + y;