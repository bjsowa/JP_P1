x == y;
a == a;
lambda x. ((lambda x. x) ((lambda x. x) (lambda z. ((lambda x. x) z)))) == lambda v. lambda s. s;
(lambda x. x) ((lambda x. x) (lambda z. ((lambda x. x) z))) == (lambda x. x);
mult 5 10 == 50;
iszero (sub 10 20) == true;
/* factorial function */
fix (lambda f. lambda n. if (iszero n) 1 (mult n (f (pred n)))) 3 == 6;
fix (lambda f. lambda n. if (iszero n) 1 (mult n (f (pred n)))) 3 == 7;
fix (lambda f. lambda n. if (iszero n) 1 (mult n (f (pred n)))) 4 == 24;